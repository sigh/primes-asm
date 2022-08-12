; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

global    _main

extern    _puts
extern    _printf

%ifndef THREADING
  %define THREADING 1
%endif
%if THREADING == 1
extern    _pthread_create
extern    _pthread_join
extern    _pthread_mutex_init
extern    _pthread_cond_init
extern    _pthread_cond_wait
extern    _pthread_cond_signal
extern    _pthread_mutex_lock
extern    _pthread_mutex_unlock
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up limits and segments.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Our limit is 10^16 because our output routine can only deal with 16 digits.
%assign MIN_SEGMENT_SIZE 128
%assign MAX_SEGMENT_SIZE 100000000
%assign MAX_LIMIT MAX_SEGMENT_SIZE*MAX_SEGMENT_SIZE
%assign PRIMES_BELOW_MAX_SEGMENT_SIZE 5761455

; Set a default limit or ensure it is not too large.
%ifndef LIMIT
  %assign LIMIT 1<<32
%endif
%if LIMIT > MAX_LIMIT
  %fatal "LIMIT is too large. Max is 10^16."
%endif

; Determine a SEGMENT_SIZE that can cover the current limit.
; Minimum segment size is 128, allowing us to process the seive in 8-byte bitset
; chunks (we skip all even numbers, thus 64 bits * 2 = 128).
%assign SEGMENT_SIZE 128
%rep 64
  %if SEGMENT_SIZE*SEGMENT_SIZE >= LIMIT
    %exitrep
  %endif
  ; Keep doubling until we reach the target.
  %assign SEGMENT_SIZE SEGMENT_SIZE*2
  ; Ensure that we don't get too large.
  %if SEGMENT_SIZE > MAX_SEGMENT_SIZE
    %assign SEGMENT_SIZE MAX_SEGMENT_SIZE
  %endif
%endrep

; Allow the segment size to be overridden.
%ifdef SEGMENT_OVERRIDE
  %if (SEGMENT_OVERRIDE)&127 != 0
    %fatal "SEGMENT_OVERRIDE must be multiple of 128"
  %endif
  %if (SEGMENT_OVERRIDE)*(SEGMENT_OVERRIDE) < LIMIT
    %fatal "SEGMENT_OVERRIDE*SEGMENT_OVERRIDE must be >= LIMIT"
  %endif
  %if (SEGMENT_OVERRIDE) > MAX_SEGMENT_SIZE
    %fatal "SEGMENT_OVERRIDE must be < 10^8"
  %endif
  %assign SEGMENT_SIZE SEGMENT_OVERRIDE
%endif

; The minimum SEGMENT_SIZE=128 which has density < 4.
%assign MAX_PRIMES_PER_SEGMENT SEGMENT_SIZE/4
; However, cap it at the max value our program is capable of reaching.
; This is necessary to get the program to compile, otherwise the memory
; allocations get too large.
%if MAX_PRIMES_PER_SEGMENT > PRIMES_BELOW_MAX_SEGMENT_SIZE
  %assign MAX_PRIMES_PER_SEGMENT PRIMES_BELOW_MAX_SEGMENT_SIZE
%endif
SEARCH_LIMIT           equ LIMIT
SEARCH_LIMIT_BITS      equ SEARCH_LIMIT/2
SEGMENT_SIZE_BITS      equ SEGMENT_SIZE/2
SEGMENT_SIZE_BYTES     equ SEGMENT_SIZE_BITS/8
; This is enough for any value less than 10^16.
; See https://en.wikipedia.org/wiki/Prime_gap for table.
MAX_PRIME_GAP          equ 1200

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants and helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%if THREADING == 1
PTHREAD_MUTEX_T_SIZE   equ 64
PTHREAD_COND_T_SIZE    equ 48
%endif

; Stack size.
STACK_VAR_BYTES        equ 80
; Stack offsets for stack variables.
SEARCH_LIMIT_BITS_VAR  equ 8
OUTPUT_LEN_VAR         equ 16
NEXT_POW_VAR           equ 24
WHEEL_SIZE_BITS_VAR    equ 32
WHEEL_DEC_BITS_VAR     equ 36
WHEEL_OFFSET_BITS_VAR  equ 40
SIEVE_PRIMES_BYTES_VAR equ 44
BCD_BUFFER_16u8_VAR    equ 48  ; 16 byte Binary-Coded Decimal output buffer.
PREV_PRIME_VAR         equ 64

WRITE_STATE_GENERATE   equ 0
WRITE_STATE_OUTPUT     equ 1

; debug <format> <value>
; Save a bunch of callee saved registers for convinience.
%macro debug 2
  ; Push the things we want to save.
  push      r8
  push      r9
  push      r10
  push      r11
  push      rax
  push      rcx
  push      rdx
  push      rdi
  push      rsi
  push      rsi
  ; Do the print
  mov       rsi, %2
  lea       rdi, [rel format_%1]
  call _printf
  ; Pop everything.
  pop       rsi
  pop       rsi
  pop       rdi
  pop       rdx
  pop       rcx
  pop       rax
  pop       r11
  pop       r10
  pop       r9
  pop       r8
%endmacro

; Copies data in 8 byte chunks.
; memcopy_q <src> <dest> <num quadwords>
%macro memcpy_q 3
  lea rsi, %1
  lea rdi, %2
  mov rcx, %3
  rep movsq
%endmacro

; Adds Binary-Coded Decimal numbers.
; scratch is used as a scratch register.
; bcd_add <dst> <src> <scratch2>
%macro bcd_add 3
  add       %1, %2                  ; Binary addition
  mov       %3, 0xF6F6F6F6F6F6F6F6  ; |
  add       %1, %3                  ; | Propogate carries into the next byte.
  mov       %2, %1
  mov       %3, 0x6060606060606060  ; |
  and       %2, %3                  ; |
  shr       %2, 4                   ; | Fix up the non-carried parts.
  mov       %3, 0x0F0F0F0F0F0F0F0F  ; |
  and       %1, %3                  ; | Isolate the carried parts.
  sub       %1, %2                  ; Combine carried and non-carried.
%endmacro

section   .text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

_main:
  push      rsp                      ; Required for alignment
  sub       rsp, STACK_VAR_BYTES

%if THREADING == 1
  call      init_writer_thread
%else
  call      write_prelude
%endif

; Create a lookup table for the Binary-Coded Decimal representation of n for
; even n up to MAX_PRIME_GAP.
; This is used to look up the BCD for prime deltas. All deltas must be even.
  lea       rdi, [rel bcd_even_lookup]
  xor       rcx, rcx                ; n = 0
  xor       rax, rax                ; bcd_n = 0
build_bcd_lookup:
  mov       [rdi+rcx*2], eax        ; bcd_even_lookup[n/2] = bcd_n
  mov       rbx, 2                  ; |
  bcd_add   rax, rbx, rdx           ; | bcd_n += bcd(2)
  add       rcx, 2                  ; n += 2
  cmp       rcx, MAX_PRIME_GAP      ; |
  jl        build_bcd_lookup        ; | if (n < MAX_PRIME_GAP) build_bcd_lookup

  ; Initialize stack variables.
  mov       rax, 1
  mov       [rsp+OUTPUT_LEN_VAR], rax
  mov       rax, 10
  mov       [rsp+NEXT_POW_VAR], rax
  mov       rax, SEARCH_LIMIT_BITS
  mov       [rsp+SEARCH_LIMIT_BITS_VAR], rax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Initialize the initial_segment_array with 1s.
  mov       rcx, SEGMENT_SIZE_BYTES
  mov       al, byte 0xFF
  lea       rdi, [rel initial_segment_array]
  rep       stosb                   ; Copy rcx copies of al to rdi.
  lea       r13, [rel initial_segment_array]
  ; Mark 1 as not a prime.
  and       [r13], byte 0xFE

  ; Initial variables used in the first segment.
  lea       r11, [rel sieve_primes]
  lea       r8, [rel segment_array]
  mov       r12, 1                  ; p = 1 (current prime)
  xor       r14, r14                ; x = 0 (byte offset into initial_segment_array)
  mov       rbx, 2                  ; w = 1 (wheel_size_bits)

; Find the primes to use for the wheel.
; Keep collecting primes until their product is too large for the first segment.
; NOTE: ALL wheel primes will be in the first quad (i.e. < 128) as a larger
;       wheel will be >10^48 (>2^160).
collect_wheel_primes:
  ; Find the LSB.
  bsf       rcx, [r13]
  ; We found a prime! Figure out the value.
  ; NOTE: Wait until confirming this prime is part of the wheel beforing
  ; unsetting it.
  lea       r12, [rcx+rcx+1]        ; | p = c*2+1
  ; Determine if we've found all the wheel primes.
  mov       rax, r12                ; |
  mul       rbx                     ; |
  cmp       rax, SEGMENT_SIZE_BITS  ; |
  jg        fill_template_array     ; | if (w*p > SEGMENT_SIZE_BITS)
                                    ; |   fill_template_array
  mov       rbx, rax                ; w = w*p
  ; Unset the LSB.
  mov       rdi, [r13]
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13], rax
  ; Move the prime into the segment array.
  xor       rax, rdi
  or        [r8], rax
  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rcx, r12                ; |
  imul      rcx, rcx                ; | c = p*p
  ; Shift c to account for the fact that the array only contains odd numbers.
  shr       rcx, 1                  ; | c /= 2

; Clear values f*p where f is even.
; This clears bits c+m*p from the segment_array at r13.
; Where c = rcx, p = r12
; clear_prime_multiples <limit>
%macro clear_prime_multiples 1
  mov       rsi, -2
.clear_prime_multiples_loop:
  mov       rdx, rsi
  mov       rax, rcx
  rol       dl, cl                  ; Important: register width MUST match shift amount.
  shr       rax, 3
  and       [r13+rax], dl           ; segment_array[c/8] &= ~(1<<(c&8))
  add       rcx, r12                ; a += p
  cmp       rcx, %1
  jl        .clear_prime_multiples_loop ; if (c < (limit)) continue
%endmacro

  clear_prime_multiples SEGMENT_SIZE_BITS
  ; NOTE: We don't store wheel primes in sieve_primes, as they are automatically
  ;       excluded by the wheel.
  jmp collect_wheel_primes

fill_template_array:
  ; Mark 1 back in as a template candidate.
  or        [r13], byte 0x01
  ; Copy the wheel to the template.
  memcpy_q  [rel initial_segment_array], \
            [rel template_segment_array], \
            SEGMENT_SIZE_BYTES/8
  ; Determine how offset the wheel is.
  xor       rdx, rdx                ; |
  mov       rax, SEGMENT_SIZE_BITS  ; |
  idiv      rbx                     ; | d = SEGMENT_SIZE_BITS%w
  ; Fill another extra rotation of the wheel to allow for any offset.
  mov       r9, rdx                 ; bit_offset = d
  lea       r10, [r9+rbx]           ; max_bit_offset = bit_offset + w
  lea       r14, [rel template_segment_array+SEGMENT_SIZE_BYTES]
  ; We fill in 4 bytes at a time.
  ; Each iteration we read 8 bytes so that we always have slack to shift in from.
fill_template_array_end:
  mov       rax, r9                 ; | Get the dword containing the wheel position.
  shr       rax, 5                  ; | index = bit_offset/32
  mov       rax, [r13+rax*4]        ; |
  mov       rcx, r9                 ; |
  and       rcx, 31                 ; |
  shr       rax, cl                 ; | a = segment_array[index*4] >> bit_offset%32
  mov       [r14], eax              ; | Align and assign dword to the template.
                                    ; | We shifted with a 64-bit value, so this is safe.
  add       r14, 4                  ; Increment BYTE pointer
  add       r9, 32                  ; Increment BIT offset
  cmp       r9, r10                 ; if (bit_offset < max_bit_offset) continue
  jle       fill_template_array_end
  ; Remove 1 again from the candidates.
  and       [r13], byte 0xFE

  ; Determine the parameters for updating the wheel offset.
  mov       [rsp+WHEEL_OFFSET_BITS_VAR], dword 0
  mov       [rsp+WHEEL_SIZE_BITS_VAR], ebx
  ; How much we need to decrement by to update the offset for the next segment.
  ; We choose decrement, as we can correct by checking if the value is negative.
  sub       rbx, rdx                ; a = w - SEGMENT_SIZE_BITS%w
  mov       [rsp+WHEEL_DEC_BITS_VAR], ebx

; Find primes for sieving (in the first segment).
  xor       r15, r15                  ; | n = 0 (offset into sieve_primes)
  mov       r14, -8                   ; | x = -8 (byte index into initial_segment_array)
  xor       rdi, rdi
collect_sieve_primes_inc:
  ; Find the next non-zero quad.
  add       r14, 8
collect_sieve_primes:
  add       rdi, [r13+r14]
  je        collect_sieve_primes_inc
  ; Find the LSB.
  bsf       rcx, rdi
  ; We found a prime! Figure out the value.
  ; NOTE: Wait until confirming this prime is part of the wheel beforing
  ; unsetting it.
  mov       rax, r14                ; |
  shl       rax, 3                  ; | convert from byte count to bit count.
  add       rcx, rax                ; |
  lea       r12, [rcx+rcx+1]        ; | p = (c+x*8)*2+1
  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rcx, r12                ; |
  imul      rcx, rcx                ; | c = p*p
  cmp       rcx, SEGMENT_SIZE
  ; If p*p is not in the segment, we have marked all the primes in the segment.
  ; So we can just directly collect the rest.
  jge       collect_large_sieve_primes  ; | if (p*p >= SEGMENT_SIZE) collect_large_sieve_primes
  ; Unset the LSB.
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13+r14], rax
  ; Move the prime into the segment array.
  xor       rax, rdi
  or        [r8+r14], rax
  ; Clear rdi for the next iteration.
  xor       rdi, rdi
  ; Shift c to account for the fact that the array only contains odd numbers.
  shr       rcx, 1                  ; | c /= 2

  clear_prime_multiples SEGMENT_SIZE_BITS

  mov       [r11+r15], rcx          ; sieve_primes[n/16].fst = f/2
  mov       [r11+r15+8], r12d       ; sieve_primes[n/16].snd = p
  add       r15, 16                 ; n += 16
  jmp       collect_sieve_primes

; Collect the rest of the primes in the first segment.
; These primes are too large to affect the first segment.
collect_large_sieve_primes:
  xor       rdi, rdi
  jmp       collect_large_sieve_primes_loop
collect_large_sieve_primes_inc:
  ; Find the next non-zero quad.
  add       r14, 8
  cmp       r14, SEGMENT_SIZE_BYTES
  jge       all_segments
collect_large_sieve_primes_loop:
  add       rdi, [r13+r14]
  je        collect_large_sieve_primes_inc
  ; Find the LSB.
  bsf       rcx, rdi
  ; Unset the LSB.
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13+r14], rax
  ; Move the prime into the segment array.
  xor       rax, rdi
  or        [r8+r14], rax
  ; Unset rdi for the next iteration.
  xor       rdi, rdi
  ; We found a prime! Figure out the value.
  mov       rax, r14                ; |
  shl       rax, 3                  ; | convert from byte count to bit count.
  add       rcx, rax                ; |
  lea       r12, [rcx+rcx+1]        ; | p = (c+x*8)*2+1
  ; This prime is inside the segment, add it to sieved_primes.
  mov       rax, r12                ; |
  mul       rax                     ; | f = p*p (next factor to look at)
  shr       rax, 1                  ; | a = f/2
  mov       [r11+r15], rax          ; sieve_primes[n/16].fst = f/2
  mov       [r11+r15+8], r12d       ; sieve_primes[n/16].snd = p
  add       r15, 16                 ; n += 16
  jmp       collect_large_sieve_primes_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rest of the segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now that we've found the sieve primes, iterate over all segments find the
; rest.
all_segments:
  ; Add a sentinal to the end of the sieve_primes array which will compare
  ; greater than any value we encounter.
  mov       rax, -1
  mov       [r11+r15], rax          ; | sieve_primes[n/16].snd = max_u64
  mov       [r11+r15+8], rax        ; | sieve_primes[n/16].fst = max_u64
  ; Reset n back to 0. We will increase it as required so we don't have to
  ; check portions of the array which aren't used yet.
  xor       r15, r15
  mov       [rsp+SIEVE_PRIMES_BYTES_VAR], r15d
  ; Initialize prev_prime to 1 so that all deltas are even.
  xor       rax, rax
  mov       rcx, 1
  mov       [rsp+BCD_BUFFER_16u8_VAR], rcx    ; |
  mov       [rsp+BCD_BUFFER_16u8_VAR+8], rax  ; | bcd_buffer = 1
  mov       [rsp+PREV_PRIME_VAR], rcx         ; | prev_prime = 1
  ; We want to use print_segment for the first segment as well.
  mov       rbx, SEGMENT_SIZE_BITS            ; segment_end_bits = segment_size_bits
  lea       r13, [rel segment_array]
  jmp       print_segment

all_segments_loop:

; Find the primes in the next segment by sieving out all of the sieve primes.
handle_segment:
  add       rbx, SEGMENT_SIZE_BITS      ; segment_end_bits += segment_size_bits
  xor       r14, r14                    ; x = 0 (index into sieve_primes)
  lea       r11, [rel sieve_primes]
  ; Update end of the relavant part of seive_primes.
  ; r15 always points to the first prime that we haven't used yet (and
  ; seive_primes has a sentinal at the end).
  mov       r15d, [rsp+SIEVE_PRIMES_BYTES_VAR]
update_sieve_primes_limit:
  add       r15, 16
  cmp       [r11+r15-16], rbx              ; We've used this element yet, so check if it still past the segment.
  jb        update_sieve_primes_limit
  sub       r15, 16                        ; We went too far, so decrement.
  mov       [rsp+SIEVE_PRIMES_BYTES_VAR], r15d
  ; Copy enough 8-byte elements to hold an offset wheel.
  mov       eax, [rsp+WHEEL_SIZE_BITS_VAR] ; |
  add       rax, SEGMENT_SIZE_BITS         ; |
  shr       rax, 3+3                       ; | (8 bits * 8 bytes)
  add       rax, 1                         ; | Padding for misalignment.
  memcpy_q  [rel template_segment_array], \
            [rel segment_array], \
            rax
  ; Increment the offset for alignment with the wheel.
  xor       eax, eax                         ; |
  mov       edx, [rsp+WHEEL_OFFSET_BITS_VAR] ; |
  mov       ecx, [rsp+WHEEL_DEC_BITS_VAR]    ; |
  sub       edx, ecx                         ; | offset -= dec
  cmovl     eax, [rsp+WHEEL_SIZE_BITS_VAR]   ; |
  add       edx, eax                         ; | if (offset < 0) offset += w
  mov       [rsp+WHEEL_OFFSET_BITS_VAR], edx ; |
  ; Initialize remaining loop registers.
  lea       r8, [rbx-SEGMENT_SIZE_BITS] ; |
  sub       r8, rdx                     ; | array_start_bits = segment_start_bits-array_offset_bits
  lea       r9, [rdx+SEGMENT_SIZE_BITS] ; | wheel_end_bits (where to stop clearing).
  lea       r13, [rel segment_array]

align 64
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  add       r14, 16                 ; x += 16
  mov       rcx, [r11+r14-16]       ; f/2 = sieve_primes[x/16].fst
  ; Check if this multiple is too large for the segment.
  ; Required because clear_primes_multiples does an unconditional first iteration.
  ; Note: Because factors increment by 2*p, we start skipping segments when the
  ;       primes get large.
  cmp       rcx, rbx
  jge       handle_segment_loop
  mov       r12d, [r11+r14-8]       ; p = sieve_primes[x/16].snd
  sub       rcx, r8                 ; c = f/2-array_start_bits

  clear_prime_multiples r9
  add       rcx, r8                 ; | Save the updated value of f back.
  mov       [r11+r14-16], rcx       ; | f/2 = c+array_start_bits
  jmp       handle_segment_loop

; Print the primes in the current segment.
print_segment:
%if THREADING == 1
  ; Wait until the writer is no longer using the print_buffer.
  mov       rdi, WRITE_STATE_GENERATE
  call      wait_until_write_state
%endif
  ; Load the (now unused) print buffer.
  lea       rsi, [rel print_buffer]          ; buf = print_buffer
  lea       r10, [rel bcd_even_lookup]
  ; Load registers from stack variables.
  mov       r11, [rsp+BCD_BUFFER_16u8_VAR]   ; |
  mov       r14, [rsp+BCD_BUFFER_16u8_VAR+8] ; | bcd_buffer
  mov       rdi, [rsp+PREV_PRIME_VAR]
  mov       r8, [rsp+OUTPUT_LEN_VAR]
  mov       r9, [rsp+NEXT_POW_VAR]
  ; Determine the wheel alignment
  mov       r15d, [rsp+WHEEL_OFFSET_BITS_VAR]
  ; Move r13 forward so that the first 8-byte section is valid.
  ; Adjust r15 to account for this.
  mov       rax, r15                  ; a = wheel_offset_bits
  and       rax, -64                  ; align a at an 8-byte boundary.
  shr       rax, 3                    ; |
  add       r13, rax                  ; | *sieve_array += a/8
  shl       rax, 3                    ; |
  ; Zero out the invalid bits of r15 bits.
  mov       rcx, r15
  sub       rcx, rax                  ; | byte_offset_bits = wheel_offset_bits-a
  shr       qword [r13], cl
  shl       qword [r13], cl
  ; Calculate the global offset
  sub       r15, rbx                  ; |
  add       r15, SEGMENT_SIZE_BITS    ; | global_offset = wheel_offset_bits - segment_start_bits

  ; Put a sentinal at the end to avoid an extra loop check.
  mov       [r13+SEGMENT_SIZE_BYTES+8], byte 0xFF

  ; If we've reached our search limit, then truncate the segment.
  ; TODO: Move this up.
  cmp       rbx, [rsp+SEARCH_LIMIT_BITS_VAR]  ; |
  cmovg     rbx, [rsp+SEARCH_LIMIT_BITS_VAR]  ; | segment_limit = min(
                                              ; |   segment_end_bits,
                                              ; |   search_limit_bits
                                              ; | )

  xor       rdx, rdx
  sub       r13, 8
align 16
print_segment_loop_inc:
  ; Find the next non-zero quad.
  add       r13, 8
print_segment_loop:
  ; NOTE: rdx must be cleared before calling print_segment_loop
  add       rdx, [r13]
  jz        print_segment_loop_inc
  ; Find the LSB.
  bsf       rcx, rdx
  ; Unset the LSB.
  lea       rax, [rdx-1]
  and       rax, rdx
  mov       [r13], rax
print_segment_found:
  ; We found a prime! Figure out the value.
  lea       rax, [rel segment_array]         ; | (let x be the current loop iteration in bytes)
  sub       rax, r13                         ; | a = -(x+wheel_offset_bits/8)
  shl       rax, 3                           ; | Convert from byte count to bit count.
  sub       rcx, rax                         ; |
  sub       rcx, r15                         ; | c = c+x*8+segment_start_bits
  cmp       rcx, rbx                         ; |
  jge       print_segment_write              ; | if (c > segment_limit) print_segment_write
  lea       r12, [rcx+rcx+1]                 ; | p = c*2+1
  ; Update output length variables.
  cmp       r12, r9                 ; | if (p >= next_pow) {
  jl        print_segment_itoa      ; |
  inc       r8                      ; |   output_len++
  shl       r9, 1                   ; |
  lea       r9, [r9+r9*4]           ; |   next_pow *= 10
  ; Convert a number to string.     ; | }
  ; Here we refer to p as b, as we destroy while outputting.
print_segment_itoa:
  mov       rdx, r12                ; | delta = p
  sub       rdx, rdi                ; | delta = p-prev
  mov       rdi, r12                ; | p = prev
  mov       eax, [r10+rdx*2]        ; bcd_delta = bcd_even_lookup[delta/2]
  ; We branch based on whether the resulting string will fit into a single register
  ; (i.e. 8 decimal digits) or if it needs 2.
  cmp       r8, 8
  jle print_segment_itoa_small
print_segment_itoa_large:
  ; Do a 16-byte BCD addition. bcd_buffer += bcd_delta
  mov       rcx, 0xF6F6F6F6F6F6F6F6 ; |
  add       r11, rax                ; |
  add       r11, rcx                ; |
  adc       r14, rcx                ; | Add with carry
  mov       rax, r11
  mov       rdx, r14
  mov       rcx, 0x6060606060606060 ; |
  and       rax, rcx                ; |
  and       rdx, rcx                ; |
  shr       rax, 4                  ; |
  shr       rdx, 4                  ; | Fix non-carried bytes
  mov       rcx, 0x0F0F0F0F0F0F0F0F ; |
  and       r11, rcx                ; |
  and       r14, rcx                ; |
  sub       r11, rax                ; |
  sub       r14, rdx                ; | Fix the carried bytes
  ; Convert from BCD to ASCII
  mov       rax, r11                ; |
  mov       rdx, r14                ; |
  bswap     rax                     ; |
  bswap     rdx                     ; | a:d = ascii_output (bcd_buffer in output byte order)
  lea       rcx, [r8*8]             ; |
  neg       rcx                     ; |
  shrd      rdx, rax, cl            ; |
  shr       rax, cl                 ; | Shift out leading 0 bytes in ascii_output
  mov       rcx, 0x3030303030303030 ; |
  or        rax, rcx                ; |
  or        rdx, rcx                ; | Add '0' to ascii_output chars
  ; Write to output buffer
  mov       [rsi], rdx              ; |
  mov       [rsi+8], rax            ; | *buf = ascii_output (16 bytes)
  mov       [rsi+r8], byte `\n`     ; |
  lea       rsi, [rsi+r8+1]         ; | Adjust buf so that only the desired
                                    ; | number of digits is kept.
  ; Clear rdx to prepare for the print_segment loop.
  xor       rdx, rdx
  jmp print_segment_loop
print_segment_itoa_small:
  ; Do an 8-byte BCD addition.
  bcd_add   r11, rax, rdx           ; bcd_buffer += bcd_delta
  ; Convert BCD to ASCII
  mov       rax, r11                ; |
  bswap     rax                     ; | ascii_output = bcd_buffer in output byte order
  lea       rcx, [r8*8]             ; |
  rol       rax, cl                 ; | Shift out leading 0 bytes in ascii_output
  mov       rcx, 0x3030303030303030 ; |
  or        rax, rcx                ; | Add '0' to ascii_output chars
  ; Write to output buffer
  mov       [rsi], rax              ; *buf = ascii_output (8 bytes)
  mov       [rsi+r8], byte `\n`     ; |
  lea       rsi, [rsi+r8+1]         ; | Adjust buf so that only the desired
                                    ; | number of digits is kept.
  ; Clear rdx to prepare for the print_segment loop.
  xor       rdx, rdx
  jmp print_segment_loop

print_segment_write:
  ; Save registers to stack variables.
  mov       [rsp+OUTPUT_LEN_VAR], r8
  mov       [rsp+NEXT_POW_VAR], r9
  mov       [rsp+BCD_BUFFER_16u8_VAR], r11
  mov       [rsp+BCD_BUFFER_16u8_VAR+8], r14
  mov       [rsp+PREV_PRIME_VAR], rdi
  ; Add a null byte to terminate the string.
  mov       [rsi-1], byte 0
%if THREADING == 1
  ; Signal the writer thread.
  lea       rdi, WRITE_STATE_OUTPUT
  call      update_and_signal_write_state
%else
  call      write_print_buffer
%endif

; Continue looping until we reach the search limit.
  cmp       rbx, [rsp+SEARCH_LIMIT_BITS_VAR]
  jl        all_segments_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finish program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

exit:
%if THREADING == 1
  ; Signal the writer thread.
  ; Ensure the last bit of output is written before we exit.
  mov       rdi, WRITE_STATE_GENERATE
  call      wait_until_write_state
%endif
  ; Fix up stack before returning
  add       rsp, STACK_VAR_BYTES
  pop       rsp
  xor       rax, rax                ; |
  ret                               ; | return 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Start the prime list manually.
; 2 has to be printed directly because the rest of the program assumes odd numbers.
write_prelude:
  push rsp
  lea       rdi, [rel prelude]
%ifndef QUIET
  call      _puts
%endif
  pop rsp
  ret

; Write print_buffer to stdout
write_print_buffer:
  push rsp
  lea       rdi, [rel print_buffer]       ; buf = print_buffer
  ; If we have nothing to write, don't    ;
  ; call _puts because it adds a newline. ;
  cmp       [rdi], byte 0                 ; | if (*buf) {
  je        .skip_write                   ; |
%ifndef QUIET                             ; |
  call _puts                              ; |   puts(buf)
%endif                                    ; | }
.skip_write:
  pop  rsp
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Threading functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%if THREADING == 1
init_writer_thread:
  push rsp
  ; Initialize signaling mutex.
                                     ; | pthread_mutex_init(
  lea       rdi, [rel writer_mutex]  ; |  mutex = writer_mutex,
  mov       rsi, 0                   ; |  attr = 0,
  call      _pthread_mutex_init      ; | )
  ; Initialize signaling condition.
                                     ; | pthread_cond_init(
  lea       rdi, [rel write_cond]    ; |  cond = write_cond,
  mov       rsi, 0                   ; |  attr = 0,
  call      _pthread_cond_init       ; | )
  ; Start writer thread
                                          ; | pthread_create(
  lea       rdi, [rel writer_thread]      ; |  thread = writer_thread,
  mov       rsi, 0                        ; |  attr = 0,
  lea       rdx, [rel writer_thread_loop] ; |  start_routine = writer_thread_loop,
  mov       rcx, 0                        ; |  arg = 0
  call      _pthread_create               ; | )
  pop rsp
  ret

; Loop forever waiting for output top be available.
writer_thread_loop:
  push      rsp
  call      write_prelude                 ; write_prelude()
.loop:                                    ; | while (true) {
  lea       rdi, WRITE_STATE_OUTPUT       ; |
  call      wait_until_write_state        ; |   block until write_state == OUTPUT
  call      write_print_buffer            ; |   write_print_buffer()
  lea       rdi, WRITE_STATE_GENERATE     ; |
  call      update_and_signal_write_state ; |   write_state = GENERATE
  jmp .loop                               ; | }

; Lock mutex and wait until write_state == rdi
; writer_mutex is locked when the function returns.
; wait_until_write_state <target_state>
wait_until_write_state:
  push      rbx
  mov       rbx, rdi
                                    ; | pthread_mutex_lock(
  lea       rdi, [rel writer_mutex] ; |  mutex = writer_mutex,
  call      _pthread_mutex_lock     ; | )
.wait_loop:                         ; | while (true) {
  lea       rax, [rel writer_state] ; |
  cmp       bl, [rax]               ; |   if (write_state == <target_state>)
  je        .done                   ; |     return
                                    ; |   pthread_cond_wait(
  lea       rdi, [rel write_cond]   ; |    cond = write_cond,
  lea       rsi, [rel writer_mutex] ; |    mutex = writer_mutex,
  call      _pthread_cond_wait      ; |   )
  jmp       .wait_loop              ; | }
.done:
  pop       rbx
  ret

; Update state, signal and unlock.
; writer_mutex is unlocked when the function returns.
; update_and_signal_write_state <new_state>
update_and_signal_write_state:
  push      rsp
  lea       rax, [rel writer_state] ; |
  mov       [rax], dil              ; | writer_state = <new_state>
                                    ; | pthread_cond_signal(
  lea       rdi, [rel write_cond]   ; |  cond = write_cond
  call      _pthread_cond_signal    ; | )
                                    ; | pthread_mutex_unlock(
  lea       rdi, [rel writer_mutex] ; |  mutex = writer_mutex,
  call      _pthread_mutex_unlock   ; | )
  pop       rsp
  ret
%endif ; THREADING == 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

section   .data

%if THREADING == 1
; pthread_t value
writer_thread:
  dq 0
; pthread_cond_t value
  align 64
write_cond:
  times PTHREAD_COND_T_SIZE db 0
; pthread_mutex_t value
  align 64
writer_mutex:
  times PTHREAD_MUTEX_T_SIZE db 0
; write_state
writer_state:
  db WRITE_STATE_GENERATE
%endif

; Small primes to directly print.
prelude:
  db '2', 0

; Format strings for debugging.
format_i64:
  db `> %ld\n`, 0
format_hex:
  db `> 0x%lx\n`, 0
format_sep:
  db `----\n`, 0

section .bss

; Template array to hold the prime wheel.
; This is used to initialize the segment_array before processing each segment.
; Space is left at the end to be able to store an extra rotations of a wheel
; plus a byte for storing a sentinal value.
  alignb 64
template_segment_array:
  resb SEGMENT_SIZE_BYTES*2+1

; The array used to sieving.
; Enough space for processing a single segment, with a buffer so that we can
; offset our use based on the position in the wheel.
  alignb 64
segment_array:
  resb SEGMENT_SIZE_BYTES
; Array used for the initial segment.
; Also acts as a buffer for segment array, so it can be offset.
initial_segment_array:
  resb SEGMENT_SIZE_BYTES+1 ; buffer

; Primes used for sieving.
  alignb 64
sieve_primes:
  ; Store pairs (f/2: u64, p: u32) where:
  ;  - f is the next candidate to be removed
  ;  - p is the prime
  ;  NOTE: It's difficult to find a way to represent f so that it will reliably
  ;        fit in 32 bits.
  ;        Storing it like this allows us to make the inner clearing loop smaller.
  ; Leave room for a sentinal at the end.
  resb (MAX_PRIMES_PER_SEGMENT+1)*16

; Lookup table for the Binary-Coded Decimal representation of even n where each
; decimal digit is 1 byte.
; Each n is stored as 4 bytes as MAX_PRIME_GAP is at most 4 decimal digits.
; bcd_even_lookup[n/2] = bcd(n)
  alignb 64
bcd_even_lookup:
  resd MAX_PRIME_GAP/2

; Buffer space to write the output string.
  alignb 64
print_buffer:
  ; The max size of each entry is 16 digits + a newline.
  resb MAX_PRIMES_PER_SEGMENT*17