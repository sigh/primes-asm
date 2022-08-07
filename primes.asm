; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

global    _main
extern    _puts
extern    _printf

%ifndef SQRT_SIZE
%define SQRT_SIZE 65535
%endif
%ifndef LIMIT
%define LIMIT SEGMENT_SIZE*SEGMENT_SIZE
%endif
; Ensure segments are 8 byte aligned.
SEGMENT_SIZE           equ ((SQRT_SIZE/128)+1)*128
SEARCH_LIMIT           equ LIMIT
ARRAY_SIZE_BITS        equ SEGMENT_SIZE/2
ARRAY_SIZE_BYTES       equ ARRAY_SIZE_BITS/8
MAX_PRIMES_PER_SEGMENT equ SEGMENT_SIZE/2

%if SEARCH_LIMIT > SEGMENT_SIZE*SEGMENT_SIZE
%error "Segment size is too small for search limit."
%endif

STACK_VAR_BYTES       equ 48
SEARCH_LIMIT_VAR      equ 8
OUTPUT_LEN_VAR        equ 16
NEXT_POW_VAR          equ 24
WHEEL_LEN_VAR         equ 32
WHEEL_DEC_VAR         equ 36
WHEEL_OFFSET_VAR      equ 40
SIEVE_PRIME_BYTES_VAR equ 44

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

section   .text

_main:
  push      rsp                     ; Required for alignment
  sub       rsp, STACK_VAR_BYTES

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
  mov       rax, SEARCH_LIMIT
  mov       [rsp+SEARCH_LIMIT_VAR], rax

  ; Initialize the initial_segment_array with 1s.
  mov       rcx, ARRAY_SIZE_BYTES
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
  mov       rbx, 2                  ; w = 2 (wheel length)

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
  cmp       rax, SEGMENT_SIZE       ; |
  jg        fill_template_array     ; | if (w*p > SEGMENT_SIZE)
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
%macro clear_prime_multiples 2
  mov       rsi, -2
  align 16
clear_prime_multiples_%1:
  mov       rdx, rsi
  mov       rax, rcx
  rol       dl, cl                  ; Important: register width MUST match shift amount.
  shr       rax, 3
  and       [r13+rax], dl           ; segment_array[c/8] &= ~(1<<(c&8))
  add       rcx, r12                ; a += p
  cmp       rcx, %2
  jl        clear_prime_multiples_%1 ; if (c < (limit)) continue
%endmacro

  clear_prime_multiples wheel_primes, ARRAY_SIZE_BITS
  ; NOTE: We don't store wheel primes in sieve_primes, as they are automatically
  ;       excluded by the wheel.
  jmp collect_wheel_primes

fill_template_array:
  ; Mark 1 back in as a template candidate.
  or        [r13], byte 0x01
  ; Copy the wheel to the template.
  memcpy_q  [rel initial_segment_array], \
            [rel template_segment_array], \
            ARRAY_SIZE_BYTES/8
  ; Determine how offset the wheel is.
  xor       rdx, rdx                ; |
  mov       rax, SEGMENT_SIZE       ; |
  idiv      rbx                     ; | d = SEGMENT_SIZE%w
  ; Fill the rest of the incomplete wheel at the end.
  mov       r9, rdx                 ; |
  shr       r9, 1                   ; | bit_offset = d/2
  lea       r14, [rel template_segment_array]
  lea       r14, [r14+ARRAY_SIZE_BYTES]
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
  cmp       r9, rbx                 ; | if (bit_offset < w) continue
  jle       fill_template_array_end

  ; Remove 1 again from the candidates.
  and       [r13], byte 0xFE

  ; Save the wheel offset.
  mov       [rsp+WHEEL_LEN_VAR], ebx

  ; Determine the parameters for updating the wheel offset.
  mov       rax, rbx                ; |
  sub       rax, rdx                ; | a = w - SEGMENT_SIZE%w

  ; The current offset of the wheel.
  mov       [rsp+WHEEL_OFFSET_VAR], dword 0
  ; How much we need to decrement by to update the offset for the next segment.
  ; We choose decrement, as we can correct by checking if the value is negative.
  mov       [rsp+WHEEL_DEC_VAR], eax

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

  clear_prime_multiples sieve_primes, ARRAY_SIZE_BITS

  lea       rcx, [rcx+rcx+1]        ; f = c*2+1
  mov       [r11+r15], rcx          ; |
  mov       [r11+r15+8], r12d       ; | sieve_primes[n/16] = (f, p)
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
  cmp       r14, ARRAY_SIZE_BYTES
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
  mov       [r11+r15], rax          ; |
  mov       [r11+r15+8], r12d       ; | sieve_primes[n/16] = (f, p)
  add       r15, 16                 ; n += 16
  jmp       collect_large_sieve_primes_loop

; Now that we've found the sieve primes, iterate over all segments find the
; rest.
all_segments:
  xor       rbx, rbx                ; segment_start = 0
  ; We want to use print_segment for the first segment as well.
  ; However, it doesn't handle single digit output so we do those manually and
  ; cross them off the list here.
  lea       r13, [rel segment_array]
  and       [r13], byte 0xF0
  mov       [rsp+SIEVE_PRIME_BYTES_VAR], r15d
  jmp       print_segment

all_segments_loop:

; Find the primes in the next segment by sieving out all of the sieve primes.
handle_segment:
  mov       r15d, [rsp+SIEVE_PRIME_BYTES_VAR]

  ; Copy enough 8-byte elements to hold an offset wheel.
  mov       eax, [rsp+WHEEL_LEN_VAR]   ; |
  add       rax, SEGMENT_SIZE          ; |
  shr       rax, 3+1+3                 ; | (8 bits * 8 bytes * 2)
  add       rax, 1                     ; | Account for truncation
  memcpy_q  [rel template_segment_array], \
            [rel segment_array], \
            rax

  ; Increment the offset for alignment with the wheel.
  xor       eax, eax                    ; |
  mov       edx, [rsp+WHEEL_OFFSET_VAR] ; |
  mov       ecx, [rsp+WHEEL_DEC_VAR]    ; |
  sub       edx, ecx                    ; | offset -= dec
  cmovl     eax, [rsp+WHEEL_LEN_VAR]    ; |
  add       edx, eax                    ; | if (offset < 0) offset += w
  mov       [rsp+WHEEL_OFFSET_VAR], edx ; |

  mov       r8, rdx                     ; |
  shr       r8, 1                       ; | array_offset_bits
  lea       r9, [r8+ARRAY_SIZE_BITS]    ; | array_end (where to stop clearing).
  lea       r13, [rel segment_array]

  xor       r14, r14                    ; x = 0 (index into sieve_primes)
  lea       r11, [rel sieve_primes]
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  mov       rcx, [r11+r14]          ; |
  mov       r12d, [r11+r14+8]       ; | (f, p) = sieve_primes[x/16]
  add       r14, 16                 ; x += 16
  ; TODO: combine r8 and rbx
  sub       rcx, rbx                ; |
  shr       rcx, 1                  ; | c = (f - segment_start)/2
  ; Check if this multiple is too large for the segment.
  cmp       rcx, ARRAY_SIZE_BITS    ;
  ; NOTE: We could do an extra check to see if we can finish this segment
  ; entirely, but currently that doesn't seem to help.
  jge       handle_segment_loop

  add       rcx, r8
  clear_prime_multiples all_segments, r9
  sub       rcx, r8                 ; |
  shl       rcx, 1                  ; | Save the updated value of f back into
  lea       rcx, [rcx+rbx+1]        ; | seive_primes
  mov       [r11+r14-16], rcx       ; | f = 2*(c+segment_start+offset)
  jmp       handle_segment_loop

; Print the primes in the current segment.
print_segment:
  lea       rsi, [rel print_buffer]   ; buf = print_buffer
  mov       r8, [rsp+OUTPUT_LEN_VAR]
  mov       r9, [rsp+NEXT_POW_VAR]
  lea       r11, [rel digit_pair_lookup]
  mov       r14, -8                   ; | x = -8 (byte index into initial_segment_array)
  xor       rdi, rdi
  ; Update  segment_start, this lets us know when to stop printing.
  add       rbx, SEGMENT_SIZE
  ; Determine the wheel alignment
  mov       r15d, [rsp+WHEEL_OFFSET_VAR]
  shr       r15, 1
  lea rax, [rel template_segment_array]
  ; Move r13 forward so that the first 8-byte section is valid.
  ; Adjust r15 to account for this.
  mov       rax, r15
  and       rax, -64                  ; align a at an 8-byte boundary.
  shr       rax, 3                    ; |
  add       r13, rax                  ; |
  shl       rax, 3                    ; | r13 += (a/8)
  ; Zero out the invalid bits of r15 bits.
  sub       r15, rax
  mov       rcx, r15
  shr       qword [r13], cl
  shl       qword [r13], cl

  ; Put a sentinal at the end
  mov       [r13+ARRAY_SIZE_BYTES+8], byte 0xFF

print_segment_loop_inc:
  ; Find the next non-zero quad.
  ; Stop at one after ARRAY_SIZE_BYTES to account for misalignment.
  add       r14, 8
print_segment_loop:
  add       rdi, [r13+r14]
  jz        print_segment_loop_inc
  ; Find the LSB.
  bsf       rcx, rdi
  ; Unset the LSB.
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13+r14], rax
  ; Clear rdi for the next iteration.
  xor       rdi, rdi
print_segment_found:
  ; We found a prime! Figure out the value.
  mov       rax, r14                    ; |
  shl       rax, 3                      ; | convert from byte count to bit count.
  add       rcx, rax                    ; |
  sub       rcx, r15                    ; |
  lea       rcx, [rcx+rcx+1]            ; |
  lea       r12, [rcx+rbx-SEGMENT_SIZE] ; |p = (c+x*8-wheel_offset)*2+1+segment_start
  cmp       r12, rbx
  jge       print_segment_write
  ; TODO: Combine this with the rbx check.
  cmp       r12, [rsp+SEARCH_LIMIT_VAR]
  jge       print_segment_write
  ; Update output length variables.
  cmp       r12, r9                 ; | if (p >= next_pow) {
  jl        print_segment_itoa      ; |
  inc       r8                      ; |   output_len++
  imul      r9, 10                  ; |   next_pow *= 10
  ; Convert a number to string.     ; | }
  ; Here we refer to p as b, as we destroy while outputting.
print_segment_itoa:
  mov       r10, rsi
  lea       rsi, [rsi+r8-2]         ; | buf += output_len - 2
  jmp       print_segment_itoa_loop_entry
print_segment_itoa_loop:
  ; Convert the last digit of b (r12) to a character (note: '0' = 48).
  lea       rcx, [rdx+rdx*4]        ; |
  lea       rcx, [rcx+rcx*4]        ; |
  shl       rcx, 2                  ; | c = d*100
  sub       r12, rcx                ; b -= c = b'%100
  mov       r12w, [r11+r12*2]       ; |
  mov       [rsi], r12w             ; | *buf = digit_pair_lookup[b]
  sub       rsi, 2                  ; buf -= 2
  mov       r12, rdx                ; b = d (b = b'/10)
print_segment_itoa_loop_entry:
  mov       rdx, r12                ; |
  mov       rax, 0x28f5c28f5c28f5c3 ; |
  shr       rdx, 2                  ; |
  mul       rdx                     ; |
  shr       rdx, 2                  ; | d = b/100
  jnz       print_segment_itoa_loop
  ; Handle the final digits
  mov       r12w, [r11+r12*2]
  mov       [rsi], r12w
  ; Finalize
  ; Note: We may write extra zeros, but it will be overwritten by the newline!
  mov       rsi, r10
  mov       [rsi-1], byte `\n`      ; | *(buf-1) = '\n'
  lea       rsi, [rsi+r8+1]         ; | buf += output_len + 1
  jmp       print_segment_loop
print_segment_write:
  ; Restore local variables.
  mov       [rsp+OUTPUT_LEN_VAR], r8
  mov       [rsp+NEXT_POW_VAR], r9
  ; If we have nothing to write, don't call _puts because it adds a newline.
  lea       rdi, [rel print_buffer]
  cmp       rsi, rdi
  je        print_segment_skip
  ; Add a null byte to terminate the string.
  mov       [rsi-1], byte 0
%ifndef QUIET
  call _puts
%endif
print_segment_skip:

; Continue looping until we reach the search limit.
  cmp       rbx, [rsp+SEARCH_LIMIT_VAR]
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

; Lookup table mapping n in [0, 100) to 2 character ascii strings.
  align 16
digit_pair_lookup:
    db "0001020304050607080910111213141516171819"
    db "2021222324252627282930313233343536373839"
    db "4041424344454647484950515253545556575859"
    db "6061626364656667686970717273747576777879"
    db "8081828384858687888990919293949596979899"

section .bss

; Template array to hold the prime wheel.
; This is used to initialize the segment_array before processing each segment.
; Space is left at the end to be able to store an extra 2 rotations of a wheel.
; TODO: Figure out why it can't just be one.
  alignb 64
template_segment_array:
  resb ARRAY_SIZE_BYTES*3

; The array used to sieving.
; Enough space for processing a single segment, with a buffer so that we can
; offset our use based on the position in the wheel.
  alignb 64
segment_array:
  resb ARRAY_SIZE_BYTES
segment_array_end:
; Array used for the initial segment.
; Also acts as a buffer for segment array, so it can be offset.
initial_segment_array:
  resb ARRAY_SIZE_BYTES ; buffer

; Primes used for sieving.
  alignb 64
sieve_primes:
  ; Store pairs (f: u64, p: u32) where:
  ;  - f is the next candidate to be removed
  ;  - p is the prime
  ;  NOTE: It's difficult to find a way to represent f so that it will reliably
  ;        fit in 32 bits.
  ;        Storing it like this allows us to make the inner clearing loop smaller.
  resb MAX_PRIMES_PER_SEGMENT*16

; Buffer space to write the output string.
  alignb 64
print_buffer:
  ; 20 bytes is enough to store 2**64
  resb MAX_PRIMES_PER_SEGMENT*20