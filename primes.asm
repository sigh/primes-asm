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

; Stack offsets for stack variables.
SEARCH_LIMIT_BITS_VAR  equ 8  ; 8 bytes
WHEEL_SIZE_BITS_VAR    equ 16 ; 8 bytes
WHEEL_DEC_BITS_VAR     equ 24 ; 4 bytes
WHEEL_OFFSET_BITS_VAR  equ 28 ; 4 bytes
SIEVE_PRIMES_BYTES_VAR equ 32 ; 8 bytes
BCD_BUFFER_16u8_VAR    equ 40 ; 16 byte Binary-Coded Decimal output buffer.
PREV_PRIME_VAR         equ 56 ; 8 bytes
; Stack size.
STACK_VAR_BYTES        equ 64

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sieve loop macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%define USE_WHEEL_OFFSETS 1
%define IGNORE_WHEEL_OFFSETS 0

; clear_prime_multiples <USE_WHEEL_OFFSETS|IGNORE_WHEEL_OFFSETS>
; Clear values f*p where f is in the mini-wheel.
; This clears bits c+f'*p from the segment_array.
; The following registers must be populated.
;   r13: segment_array
;   r12: p    - prime being sieved
;   rcx: c    - offset of next candidate
;   r9: limit - loop while (c < limit)
;   rsi: i    -  index into mini_wheel_deltas
; The following registers are used (and not restored):
;   rax, rdx, rdi
%macro clear_prime_multiples 1
.fill_mini_wheel_deltas:
  ; Fill mini-wheel with the appropriate prime multiples.
  ; The mini-wheel has 30 numbers, which has length 15 since we only store odd numbers.
    ; Wheel offsets: 1, 7, 11, 13, 17, 19, 23, 29
    ; Divide by 2  : 0, 3, 5,  6,  8,  9,  11, 14
    ; Deltas       : 3, 2, 1,  2,  1,  2,  3,  1
  %if %1 == USE_WHEEL_OFFSETS
    lea       rdi, [rel mini_wheel_deltas]
    lea       rax, [r12+r12]
    ; p
    mov       [rdi+8*2], r12
    mov       [rdi+8*4], r12
    mov       [rdi+8*7], r12
    ; p*2
    mov       [rdi+8*1], rax
    mov       [rdi+8*3], rax
    mov       [rdi+8*5], rax
    ; p*3
    lea       rax, [rax+r12]
    mov       [rdi+8*0], rax
    mov       [rdi+8*6], rax
  %endif
.clear_prime_multiples_loop:
  mov       rdx, -2                 ; |
  mov       rax, rcx                ; |
  rol       dl, cl                  ; | d = ~(1<<(c&8)) (Important: register width MUST match shift amount.)
  shr       rax, 3                  ; |
  and       [r13+rax], dl           ; | segment_array[c/8] &= ~(1<<(c&8))
  %if %1 == USE_WHEEL_OFFSETS
    add       rcx, [rdi+rsi*8]      ; a += mini_wheel_deltas[i] (a += k*p)
    add       esi, 1                ; |
    and       esi, 7                ; | i = (i+1)%8
  %else
    add       rcx, r12              ; a += p
  %endif
  cmp       rcx, r9                     ; |
  jl        .clear_prime_multiples_loop ; | if (c < (limit)) continue
%endmacro

; Determine the starting offset for the mini-wheel.
; The input to this is always a prime <p> the wheel shows the offset of the
; value we are multipying by, and we always start at p*p.
; mini_wheel_position <result> <p>
%macro mini_wheel_position 2
    mov     rax, 9838263505978427529 ; |
    mul     %2                       ; |
    shr     rdx, 4                   ; | d = p/30
    mov     rax, %2                  ; |
    shr     rax, 1                   ; | a = p/2
    lea     rdx, [rdx + 4*rdx]       ; |
    lea     rdx, [rdx + 2*rdx]       ; | d = p/30*15
    sub     rax, rdx                 ; a = (p%30)/2 = (p/2)%15 = p/2 - (p/2)/15*15
    ; result = mini_wheel_position_lookup[a]
    lea     rdx, [rel mini_wheel_position_lookup]
    movsx   %1, byte [rdx+rax]
    ; result == -1 indicates we have a bug.
    cmp     %1, -1
    je      fatal
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

section   .text

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
  ; Increment by 2
  add       rax, 2                  ; | Binary a+2
  mov       rdx, 0xF6F6F6F6         ; | |
  add       rax, rdx                ; | | Propogate carries into the next byte.
  mov       rbx, rax                ; |
  mov       rdx, 0x60606060         ; | |
  and       rbx, rdx                ; | |
  shr       rbx, 4                  ; | | Create the adjustment for non-carried bytes.
  mov       rdx, 0x0F0F0F0F         ; | |
  and       rax, rdx                ; | | Clear the carry information.
  sub       rax, rbx                ; | Adjust for non-carried bytes.
                                    ; | bcd_n += bcd(2)
  add       rcx, 2                  ; n += 2
  cmp       rcx, MAX_PRIME_GAP      ; |
  jl        build_bcd_lookup        ; | if (n < MAX_PRIME_GAP) build_bcd_lookup

  ; Initialize stack variables.
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
  ; NOTE: Wait until confirming this prime is part of the wheel before
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
  xor       rax, rdi                ; extract the bit
  or        [r8], rax               ; insert it into the segment array (wheel primes are always in the first bitset)
  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rcx, r12                ; |
  imul      rcx, rcx                ; | c = p*p
  ; Shift c to account for the fact that the array only contains odd numbers.
  shr       rcx, 1                  ; | c /= 2

  mov       r9, SEGMENT_SIZE_BITS
  ; NOTE: We ignore wheel offsets because some of these primes will be in the
  ;       mini-wheel.
  clear_prime_multiples IGNORE_WHEEL_OFFSETS
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
collect_sieve_primes_inc:
  ; Find the next non-zero quad.
  add       r14, 8
  cmp       r14, SEGMENT_SIZE_BYTES
  jge       all_segments
collect_sieve_primes:
  cmp       qword [r13+r14], 0
  je        collect_sieve_primes_inc
  ; Find the LSB.
  mov       rdi, [r13+r14]
  bsf       rcx, rdi
  ; Unset the LSB.
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13+r14], rax
  ; We found a prime! Move it into the segment array.
  xor       rax, rdi
  or        [r8+r14], rax
  ; Figure out the value of the prime.
  mov       rax, r14                ; |
  shl       rax, 3                  ; | convert from byte count to bit count.
  add       rcx, rax                ; |
  lea       r12, [rcx+rcx+1]        ; | p = (c+x*8)*2+1
  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rcx, r12                ; |
  imul      rcx, rcx                ; | c = p*p
  ; Shift c to account for the fact that the array only contains odd numbers.
  shr       rcx, 1                  ; c /= 2

  mini_wheel_position rsi, r12
  ; If p*p is not in the segment, then we shouldn't clear anything.
  cmp       rcx, SEGMENT_SIZE_BITS  ; |
  jge       add_sieve_prime         ; | if (p*p >= SEGMENT_SIZE) add_sieve_prime

  mov       r9, SEGMENT_SIZE_BITS
  clear_prime_multiples USE_WHEEL_OFFSETS

add_sieve_prime:
  mov       [r11+r15], rcx          ; sieve_primes[n/16].fst = m/2
  mov       [r11+r15+8], r12d       ; sieve_primes[n/16].snd = p
  mov       [r11+r15+12], esi       ; sieve_primes[n/16].trd = i
  add       r15, 16                 ; n += 16
  jmp       collect_sieve_primes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rest of the segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%macro common_handle_segment_loop_prologue 0
  ; Add a sentinel to the end of segment_array so that there can be fewer
  ; loop condition checks.
  mov       [r13+SEGMENT_SIZE_BYTES+8], byte 0xFF
  ; If we've reached our search limit, then truncate the segment.
  cmp       rbx, [rsp+SEARCH_LIMIT_BITS_VAR]  ; |
  cmovg     rbx, [rsp+SEARCH_LIMIT_BITS_VAR]  ; | segment_limit = min(
                                              ; |   segment_end_bits,
                                              ; |   search_limit_bits
                                              ; | )
%endmacro

; Now that we've found the sieve primes, iterate over all segments find the
; rest.
all_segments:
  ; Add a sentinel to the end of the sieve_primes array which will compare
  ; greater than any value we encounter.
  mov       rax, -1
  mov       [r11+r15], rax          ; | sieve_primes[n/16].fst = max_u64
  mov       [r11+r15+8], rax        ; | sieve_primes[n/16].{snd,trd} = max_u64
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
  ; Prepare segment offsets.
  mov       rbx, SEGMENT_SIZE_BITS            ; segment_end_bits = segment_size_bits
  ; Set up preparations for print_segment.
  lea       r13, [rel segment_array]
  mov       r8, 0                             ; array_start_bits = 0
  common_handle_segment_loop_prologue
  ; We want to use print_segment for the first segment as well.
  jmp       print_segment

all_segments_loop:

; Find the primes in the next segment by sieving out all of the sieve primes.
handle_segment:
  add       rbx, SEGMENT_SIZE_BITS      ; segment_end_bits += segment_size_bits
  xor       r14, r14                    ; x = 0 (index into sieve_primes)
  lea       r11, [rel sieve_primes]
  ; Update end of the relavant part of seive_primes.
  ; r15 always points to the first prime that we haven't used yet (and
  ; seive_primes has a sentinel at the end).
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
  ; Initialize r13 as segment_array.
  ; Move r13 forward so that the first 8-byte section is valid.
  ; Adjust offset to account for this.
  lea       r13, [rel segment_array]
  mov       rax, rdx                  ; a = wheel_offset_bits
  and       rax, -64                  ; align a at an 8-byte boundary.
  shr       rax, 3                    ; |
  add       r13, rax                  ; | *sieve_array += a/8
  shl       rax, 3                    ; |
  sub       rdx, rax                  ; | byte_offset_bits = wheel_offset_bits-a
  ; Zero out the bits in [r13] before the segment start.
  ; This is required so that they aren't considered primes later.
  mov       rcx, rdx                  ; |
  shr       qword [r13], cl           ; |
  shl       qword [r13], cl           ; | Clear the first CL bits.
  ; Initialize remaining loop registers.
  lea       r8, [rbx-SEGMENT_SIZE_BITS] ; |
  sub       r8, rdx                     ; | array_start_bits = segment_start_bits-array_offset_bits
  lea       r9, [rdx+SEGMENT_SIZE_BITS] ; | wheel_end_bits (where to stop clearing).
  ; Common pre-loop setup
  common_handle_segment_loop_prologue

align 64
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  add       r14, 16                 ; x += 16
  mov       rcx, [r11+r14-16]       ; m/2 = sieve_primes[x/16].fst
  ; Check if this multiple is too large for the segment.
  ; Required because clear_primes_multiples does an unconditional first iteration.
  ; Note: Because multiples increment by 2*p, we start skipping segments when
  ;       the primes get large.
  cmp       rcx, rbx
  jge       handle_segment_loop
  mov       r12d, [r11+r14-8]       ; p = sieve_primes[x/16].snd
  mov       esi, [r11+r14-4]        ; i = sieve_primes[x/16].trd
  sub       rcx, r8                 ; c = m/2-array_start_bits

  clear_prime_multiples USE_WHEEL_OFFSETS
  add       rcx, r8                 ; | Save the updated value of m back.
  mov       [r11+r14-16], rcx       ; | m/2 = c+array_start_bits
  mov       [r11+r14-4], esi        ; | i
  jmp       handle_segment_loop

; Print the primes in the current segment.
print_segment:
  ; Use r15 for array_start_bits
  mov       r15, r8
%if THREADING == 1
  ; Wait until the writer is no longer using the print_buffer.
  mov       rdi, WRITE_STATE_GENERATE
  call      wait_until_write_state
%endif
  ; Load the (now unused) print buffer.
  lea       rsi, [rel print_buffer]          ; buf = print_buffer
  ; Load registers from stack variables.
  mov       r11, [rsp+BCD_BUFFER_16u8_VAR]   ; |
  mov       r14, [rsp+BCD_BUFFER_16u8_VAR+8] ; | bcd_buffer
  mov       rdi, [rsp+PREV_PRIME_VAR]
  ; Initialize loop registers.
  mov       r9, -8                  ; x = -8 (x will start at 0).
print_segment_loop_inc:
  ; Find the next non-zero quad.
  add       r9, 8                   ; x += 8
  mov       r10, [r13+r9]           ; current_value = array[x]
print_segment_loop:
  cmp       r10, 0
  je        print_segment_loop_inc
  ; Find the LSB.
  bsf       rcx, r10
  ; Unset the LSB.
  lea       rax, [r10-1]
  and       r10, rax
print_segment_found:
  ; We found a prime! Figure out the value.
  lea       rcx, [rcx+r9*8]                  ; |
  add       rcx, r15                         ; | c = c+x*8+array_start_bits
  cmp       rcx, rbx                         ; |
  jge       print_segment_write              ; | if (c > segment_limit) print_segment_write
  lea       r12, [rcx+rcx+1]                 ; | p = c*2+1
  ; Convert the prime to a string.
print_segment_itoa:
  mov       rdx, r12                ; | delta = p
  sub       rdx, rdi                ; | delta = p-prev
  mov       rdi, r12                ; | p = prev
  lea       rax, [rel bcd_even_lookup] ; |
  mov       eax, [rax+rdx*2]           ; | bcd_delta = bcd_even_lookup[delta/2]
  ; Do a 16-byte BCD addition. bcd_buffer += bcd_delta
  mov       rcx, 0xF6F6F6F6F6F6F6F6 ; |
  add       r11, rax                ; | Ensure carries propogate to the next byte.
  add       r11, rcx                ; |
  adc       r14, rcx                ; | (Binary) add with carry
  mov       rax, r11
  mov       rdx, r14
  mov       rcx, 0x6060606060606060 ; |
  and       rax, rcx                ; |
  and       rdx, rcx                ; |
  shr       rax, 4                  ; |
  shr       rdx, 4                  ; | Create the adjustment for non-carried bytes.
  mov       rcx, 0x0F0F0F0F0F0F0F0F ; |
  and       r11, rcx                ; |
  and       r14, rcx                ; | Clear the carry information.
  sub       r11, rax                ; |
  sub       r14, rdx                ; | Adjust for non-carried bytes.
  jz        print_segment_convert_8byte
print_segment_convert_16byte:
  ; Convert from BCD to ASCII for all 16 bytes
  bsr       r8, r14                 ; c = index of most significant bit
  shr       r8, 3                   ; c = index of most significant byte
  add       r8, 1+8                 ; c = #digits in bcd_buffer
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
  jmp print_segment_loop
print_segment_convert_8byte:
  ; Convert BCD to ASCII for just the lower 8 bytes
  bsr       r8, r11                 ; c = index of most significant bit
  shr       r8, 3                   ; c = index of most significant byte
  add       r8, 1                   ; c = #digits in bcd_buffer
  mov       rax, r11                ; |
  bswap     rax                     ; | ascii_output = bcd_buffer in output byte order
  lea       rcx, [r8*8]             ; |
  rol       rax, cl                 ; | Shift out leading 0 bytes in ascii_output
  mov       rdx, 0x3030303030303030 ; |
  or        rax, rdx                ; | Add '0' to ascii_output chars
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

fatal:
  debug     fatal, rax
  jmp       exit

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

; Array to populate wheel deltas. In the clearance loop, this is pre-populated
; multiples of the primes we are clearing.
  align 64
mini_wheel_deltas:
  times 8 dq 0

; Lookup mini-wheel position given (p%30)/2.
; The mini-wheel has 30 numbers, which has length 15 since we only store odd numbers.
   ; Wheel offsets: 1, 7, 11, 13, 17, 19, 23, 29
   ; Divide by 2  : 0, 3, 5,  6,  8,  9,  11, 14
   ; Deltas       : 3, 2, 1,  2,  1,  2,  3,  1
mini_wheel_position_lookup:
  ; -1s indicate invalid values.
  db 0   ; 0
  db -1  ;
  db -1  ;
  db 1   ; 3
  db -1  ;
  db 2   ; 5
  db 3   ; 6
  db -1  ;
  db 4   ; 8
  db 5   ; 9
  db -1  ;
  db 6   ; 11
  db -1  ;
  db -1  ;
  db 7   ; 14

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
format_fatal:
  db `Fatal error: %ld\n`, 0

section .bss

; Template array to hold the prime wheel.
; This is used to initialize the segment_array before processing each segment.
; Space is left at the end to be able to store an extra rotations of a wheel
; plus a byte for storing a sentinel value.
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
  ; Store pairs (m/2: u64, p: u32, i: u32) where:
  ;  - m is the next candidate to be removed
  ;  - p is the prime
  ;  - i is the mini-wheel index: i in range [0, 15)
  ;  NOTE: It's difficult to find a way to represent m so that it will reliably
  ;        fit in 32 bits.
  ;        Storing it like this allows us to make the inner clearing loop smaller.
  ; Leave room for a sentinel at the end.
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