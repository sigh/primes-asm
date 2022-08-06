; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

global    _main
extern    _puts
extern    _printf

%ifndef SQRT_SIZE
%define SQRT_SIZE 10_000
%endif
SEGMENT_SIZE  equ SQRT_SIZE
SEARCH_LIMIT  equ SEGMENT_SIZE*SEGMENT_SIZE
ARRAY_SIZE    equ SEGMENT_SIZE/2

STACK_VAR_BYTES     equ 48
SEARCH_LIMIT_VAR    equ 8
OUTPUT_LEN_VAR      equ 16
NEXT_POW_VAR        equ 24
WHEEL_LEN_VAR       equ 32
WHEEL_DEC_VAR       equ 40
WHEEL_OFFSET_VAR    equ 44

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
  lea       rdi, [rel %1]
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

%macro memcpy 3
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
  call      _puts

  ; Initialize stack variables.
  mov       rax, 1
  mov       [rsp+OUTPUT_LEN_VAR], rax
  mov       rax, 10
  mov       [rsp+NEXT_POW_VAR], rax
  mov       rax, SEARCH_LIMIT/2
  mov       [rsp+SEARCH_LIMIT_VAR], rax

  ; Initialize the candidate_array with 1s.
  mov       rcx, ARRAY_SIZE
  mov       al, byte 1
  lea       rdi, [rel candidate_array]
  rep       stosb                   ; Copy rcx copies of al to rdi.
  ; r13 refers to the end of the array so that loop termination can just check
  ; against 0 instead of doing an explicit comparision.
  lea       r13, [rel candidate_array_end]
  ; Sentinal value so that candidate loops don't need to check for ARRAY_SIZE
  mov       [r13], byte 1

  ; Initial variables used in the first segment.
  lea       r11, [rel sieve_primes]
  mov       r12, 1                  ; p = 1
  xor       r15, r15                ; n = 0
  xor       r14, r14                ; x = 0
  mov       rbx, 1                  ; w = 1

; Find the primes to use for the wheel.
; Keep collecting primes until their product is too large for the first segment.
collect_wheel_primes:
  add       r14, 1                  ; x++
  add       r12, 2                  ; p += 2
  cmp       [r13+r14-ARRAY_SIZE], byte 0
  je        collect_wheel_primes  ; if (candidate_array[x] == 0) collect_wheel_primes

  mov       rax, r12                ; |
  mul       rbx                     ; |
  cmp       rax, ARRAY_SIZE         ; |
  jg        fill_template_candidate_array ; | if (w*p > ARRAY_SIZE)
                                          ; |   fill_template_candidate_array
  mov       rbx, rax                ; w = w*p

  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rax, r12                ; |
  mul       rax                     ; | a = p*p
  ; Shift a to account for the fact that the array only contains odd numbers.
  ; Make it a negative offset as required by the clear_prime_multiples loop.
  shr       rax, 1                  ; |
  sub       rax, ARRAY_SIZE         ; | a = (p*p)/2 - ARRAY_SIZE

; Clear values f*p where f is even.
; This clears entries a+m*p from the candidate_array at r13.
; a must be a negative index, so that we exit the loop when it reaches 0.
; Where a = rax, p = r12
%macro clear_prime_multiples 1
clear_prime_multiples_%1:
  mov       [r13+rax], byte 0       ; candidate_array[a] = 0
  add       rax, r12                ; a += p
  jl        clear_prime_multiples_%1 ; if (a < 0) continue
%endmacro

  clear_prime_multiples wheel_primes

  ; NOTE: We don't store wheel primes in sieve_primes, as they are automatically
  ;       excluded by the wheel.
  jmp collect_wheel_primes

fill_template_candidate_array:
  ; Copy the wheel to the template.
  lea       rdx, [rel template_candidate_array]
  memcpy    [rel candidate_array], \
            [rdx], \
            ARRAY_SIZE/8+1
  ; Make a second copy offset by the wheel size, to account for offsets.
  memcpy    [rel candidate_array], \
            [rdx+rbx], \
            ARRAY_SIZE/8+1

  ; We need to reprocess the current prime, as it is not part of the wheel.
  sub r14, 1
  sub r12, 2

  ; Clear the wheel primes from the template array.
  ; We want to keep them in the candidate array so that they get printed
  ; out.
  mov       al, byte 0
  mov       rcx, r14
  lea       rdi, [rdx+1]
  rep       stosb
  mov       rcx, r14
  lea       rdi, [rdx+rbx+1]
  rep       stosb

  ; Save the wheel offset.
  mov       [rsp+WHEEL_LEN_VAR], rbx

  ; Determine the parameters for updating the wheel offset.
  xor       rdx, rdx                ; |
  mov       rax, ARRAY_SIZE         ; |
  idiv      rbx                     ; | d = ARRAY_SIZE%w
  mov       rax, rbx                ; |
  sub       rax, rdx                ; | a = w - ARRAY_SIZE%w

  ; The current offset of the wheel.
  mov       [rsp+WHEEL_OFFSET_VAR], dword 0
  ; How much we need to decrement by to update the offset for the next segment.
  ; We choose decrement, as we can correct by checking if the value is negative.
  mov       [rsp+WHEEL_DEC_VAR], eax

; Find primes for sieving (in the first segment).
collect_sieve_primes:
  add       r14, 1                  ; x++
  add       r12, 2                  ; p += 2
  cmp       [r13+r14-ARRAY_SIZE], byte 0
  je        collect_sieve_primes  ; if (candidate_array[x] == 0) collect_sieve_primes

  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rax, r12                ; a = p
  mul       rax                     ; a = a*a
  ; Shift a to account for the fact that the array only contains odd numbers.
  ; Make it a negative offset as required by the clear_prime_multiples loop.
  shr       rax, 1                  ; |
  sub       rax, ARRAY_SIZE         ; | a = (p*p)/2 - ARRAY_SIZE
  ; If p*p is past the end of the array, we have marked all the primes in the segment.
  ; So we can just directly collect the rest.
  jge       collect_large_sieve_primes  ; | if (p*p/2 >= ARRAY_SIZE) collect_large_sieve_primes

  clear_prime_multiples sieve_primes

  add       rax, ARRAY_SIZE         ; |
  mov       [r11+r15], rax          ; |
  mov       [r11+r15+8], r12d       ; | sieve_primes[n/16] = (f/2, p)
  add       r15, 16                 ; n += 16
  jmp       collect_sieve_primes

; Collect the rest of the primes in the first segment.
; These primes are too large to affect the first segment.
collect_large_sieve_primes:
  sub       r14, 1
collect_large_sieve_primes_loop:
  ; NOTE: Because we have a sentinal element, we don't need to check the loop bounds here.
  add       r14, 1
  cmp       [r13+r14-ARRAY_SIZE], byte 0
  je        collect_large_sieve_primes_loop ; if (candidate_array[x] == 0) collect_large_sieve_primes_loop
collect_large_sieve_primes_found:
  cmp       r14, ARRAY_SIZE         ; |
  jge       all_segments            ; | if (x >= ARRAY_SIZE) goto all_segments
  lea       r12, [r14*2+1]          ; p = x*2 + 1
  mov       rax, r12                ; |
  mul       rax                     ; | f = p*p (next factor to look at)
  shr       rax, 1                  ; |
  mov       [r11+r15], rax          ; |
  mov       [r11+r15+8], r12d       ; | sieve_primes[n/16] = (f/2, p)
  add       r15, 16                 ; n += 16
  jmp       collect_large_sieve_primes_loop

; Now that we've found the sieve primes, iterate over all segments find the
; rest.
all_segments:
  xor       rbx, rbx                ; segment_start = 0
  ; We want to use print_segment for the first segment as well.
  ; However, it doesn't handle single digit output so we do those manually and
  ; cross them off the list here.
  mov       [r13-ARRAY_SIZE], dword 0
  jmp       print_segment

all_segments_loop:

; Find the primes in the next segment by sieving out all of the sieve primes.
handle_segment:
  ; Copy enough 8-byte elements to hold an offset wheel.
  mov       rax, [rsp+WHEEL_LEN_VAR]   ; |
  shr       rax, 3                     ; |
  add       rax, ARRAY_SIZE/8+1        ; | a = (w+ARRAY_SIZE)/8+1
  memcpy    [rel template_candidate_array], \
            [rel candidate_array], \
            rax

  ; Increment the offset for alignment with the wheel.
  xor       eax, eax                    ; |
  mov       edx, [rsp+WHEEL_OFFSET_VAR] ; |
  mov       ecx, [rsp+WHEEL_DEC_VAR]    ; |
  sub       edx, ecx                    ; | offset -= dec
  cmovl     rax, [rsp+WHEEL_LEN_VAR]    ; |
  add       edx, eax                    ; | if (offset < 0) offset += w
  mov       [rsp+WHEEL_OFFSET_VAR], edx ; |

  ; Align candidate_array with the wheel.
  ; (we have padding at the end so it's ok to go over).
  lea       r13, [rel candidate_array_end]
  add       r13, rdx
  ; Sentinal value so that candidate loops don't need to check for ARRAY_SIZE
  mov       [r13], byte 1

  xor       r14, r14                ; x = 0
  lea       r11, [rel sieve_primes]
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  mov       rax, [r11+r14]          ; |
  mov       r12d, [r11+r14+8]       ; | (p, f/2) = sieve_primes[x/16]
  add       r14, 16                 ; x += 16
  sub       rax, rbx                ; a = f/2 - segment_start
  ; Check if this multiple is too large for the segment.
  sub       rax, ARRAY_SIZE         ; a = f/2 - segment_start - array_size
  ; NOTE: We could do an extra check to see if we can finish this segment
  ; entirely, but currently that doesn't seem to help.
  jge       handle_segment_loop

  clear_prime_multiples all_segments
  lea       rax, [rax+rbx+ARRAY_SIZE] ; | Add back in the segment_start and array_size
  mov       [r11+r14-16], rax         ; | Save the updated value of f/2
  jmp       handle_segment_loop

; Print the primes in the current segment.
print_segment:
  xor       r14, r14                ; x = 0
  lea       rsi, [rel print_buffer] ; buf = print_buffer
  lea       rdi, [rel print_buffer]
  mov       r8, [rsp+OUTPUT_LEN_VAR]
  mov       r9, [rsp+NEXT_POW_VAR]
  lea       r11, [rel digit_pair_lookup]
print_segment_loop:
  ; Keep looping until candidate_array[x-1] != 0
  ; Note: candidate_array has a sentinal, so we don't need to check the loop condition.
  add       r14, 1
  cmp       [r13+r14-1-ARRAY_SIZE], byte 0
  je        print_segment_loop
print_segment_found:
  cmp       r14, ARRAY_SIZE         ; |
  jg        print_segment_write     ; | if (x > ARRAY_SIZE) print_segment_write
  lea       r12, [rbx+r14]          ; |
  lea       r12, [r12+r12-1]        ; | p = (segment_start + x)*2 + 1
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
  cmp       rsi, rdi
  je        print_segment_skip
  ; Add a newline to the end of the string.
  mov       [rsi-1], byte 0
  call _puts
print_segment_skip:

; Continue looping until we reach the search limit.
  add       rbx, ARRAY_SIZE
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
format_u64:
  db `> %ld\n`, 0
sep:
  db `----\n`, 0

  align 16
digit_pair_lookup:
    dw 0x3030, 0x3130, 0x3230, 0x3330, 0x3430, 0x3530, 0x3630, 0x3730, 0x3830, 0x3930
    dw 0x3031, 0x3131, 0x3231, 0x3331, 0x3431, 0x3531, 0x3631, 0x3731, 0x3831, 0x3931
    dw 0x3032, 0x3132, 0x3232, 0x3332, 0x3432, 0x3532, 0x3632, 0x3732, 0x3832, 0x3932
    dw 0x3033, 0x3133, 0x3233, 0x3333, 0x3433, 0x3533, 0x3633, 0x3733, 0x3833, 0x3933
    dw 0x3034, 0x3134, 0x3234, 0x3334, 0x3434, 0x3534, 0x3634, 0x3734, 0x3834, 0x3934
    dw 0x3035, 0x3135, 0x3235, 0x3335, 0x3435, 0x3535, 0x3635, 0x3735, 0x3835, 0x3935
    dw 0x3036, 0x3136, 0x3236, 0x3336, 0x3436, 0x3536, 0x3636, 0x3736, 0x3836, 0x3936
    dw 0x3037, 0x3137, 0x3237, 0x3337, 0x3437, 0x3537, 0x3637, 0x3737, 0x3837, 0x3937
    dw 0x3038, 0x3138, 0x3238, 0x3338, 0x3438, 0x3538, 0x3638, 0x3738, 0x3838, 0x3938
    dw 0x3039, 0x3139, 0x3239, 0x3339, 0x3439, 0x3539, 0x3639, 0x3739, 0x3839, 0x3939

section .bss

  alignb 64
template_candidate_array:
  resb ARRAY_SIZE*2

  alignb 64
candidate_array:
  resb ARRAY_SIZE
candidate_array_end:
  resb ARRAY_SIZE ; buffer

  alignb 64
; Primes used for sieving.
sieve_primes:
  ; TODO: Can these be packed further by storing deltas?
  ; Store pairs (f/2: u64, p: u32) where:
  ;  - f is the next candidate to be removed
  ;  - p is the prime
  ;  NOTE: It's difficult to find a way to represent f so that it will reliably
  ;        fit in 32 bits.
  ;        Storing it like this allows us to make the inner clearing loop smaller.
  resb ARRAY_SIZE*16

  alignb 64
print_buffer:
  ; 20 bytes is enough to store 2**64
  resb ARRAY_SIZE*20