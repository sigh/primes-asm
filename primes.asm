; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

global    _main
extern    _puts
extern    _printf

%ifndef SEGMENT_HINT
%define SEGMENT_HINT 65535
%endif
%ifndef LIMIT
%define LIMIT SEGMENT_SIZE*SEGMENT_SIZE
%endif
; Ensure segments are 8 byte aligned.
SEGMENT_SIZE           equ ((SEGMENT_HINT/128)+1)*128
SEARCH_LIMIT_BITS      equ LIMIT/2
SEGMENT_SIZE_BITS      equ SEGMENT_SIZE/2
SEGMENT_SIZE_BYTES     equ SEGMENT_SIZE_BITS/8
; The minimum SEGMENT_SIZE=128 which has density < 4.
MAX_PRIMES_PER_SEGMENT equ SEGMENT_SIZE/4
; This is enough for any value less than 10^16.
; See https://en.wikipedia.org/wiki/Prime_gap for table.
MAX_PRIME_GAP          equ 1200

%if LIMIT > SEGMENT_SIZE*SEGMENT_SIZE
%error "LIMIT is too large for segment size. Set a larger SEGMENT_HINT."
%endif

; Our limit is 10^16 because our output routine can only deal with 16 digits.
%if LIMIT > 10000000000000000
%error "LIMIT is too large. Max is 10^16."
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

_main:
  push      rsp                     ; Required for alignment
  sub       rsp, STACK_VAR_BYTES

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

initialize:
  ; Write out the first primes directly.
  ;  - 2 has to be printed directly because the rest of the program assumes odd
  ;    numbers.
  ;  - The single digit primes are printed out as the itoa code doesn't handle
  ;    them.
  lea       rdi, [rel prelude]
%ifndef QUIET
  call      _puts
%endif

  ; Initialize stack variables.
  mov       rax, 1
  mov       [rsp+OUTPUT_LEN_VAR], rax
  mov       rax, 10
  mov       [rsp+NEXT_POW_VAR], rax
  mov       rax, SEARCH_LIMIT_BITS
  mov       [rsp+SEARCH_LIMIT_BITS_VAR], rax

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
  ; However, it doesn't handle single digit output so we do those manually and
  ; cross them off the list here.
  mov       rbx, SEGMENT_SIZE_BITS            ; segment_end_bits = segment_size_bits
  lea       r13, [rel segment_array]
  and       [r13], byte 0xF0
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
  ; If we have nothing to write, don't call _puts because it adds a newline.
  lea       rdi, [rel print_buffer]
  cmp       rsi, rdi
  je        print_segment_skip

%ifndef QUIET
  ; Add a null byte to terminate the string.
  mov       [rsi-1], byte 0
  call _puts
%endif
print_segment_skip:

; Continue looping until we reach the search limit.
  cmp       rbx, [rsp+SEARCH_LIMIT_BITS_VAR]
  jl        all_segments_loop

exit:
  add       rsp, STACK_VAR_BYTES
  pop       rsp                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

section   .data

; Small primes to directly print.
prelude:
  db `2\n3\n5\n7`, 0

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