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

LOCAL_VAR_BYTES     equ 48
SEARCH_LIMIT_OFFSET equ 8
OUTPUT_LEN_OFFSET   equ 16
NEXT_POW_OFFSET     equ 24
WHEEL_LEN_OFFSET    equ 32
WHEEL_DEC_OFFSET  equ 40
WHEEL_OFFSET_OFFSET equ 44

; debug <format> <value>
; Save a bunch of callee saved registers for convinience.
%macro debug 2
  ; Push the things we want to save.
  push      r11
  push      r12
  push      r13
  push      r14
  push      r15
  push      rax
  push      rcx
  push      rdx
  push      rdi
  push      rsi
  ; Do the print
  mov       rsi, %2
  lea       rdi, [rel %1]
  call _printf
  ; Pop everything.
  pop       rsi
  pop       rdi
  pop       rdx
  pop       rcx
  pop       rax
  pop       r15
  pop       r14
  pop       r13
  pop       r12
  pop       r11
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
  sub       rsp, LOCAL_VAR_BYTES

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
  mov       [rsp+OUTPUT_LEN_OFFSET], rax
  mov       rax, 10
  mov       [rsp+NEXT_POW_OFFSET], rax
  mov       rax, SEARCH_LIMIT/2
  mov       [rsp+SEARCH_LIMIT_OFFSET], rax

  ; Initialize the candidate_array with 1s.
  mov       rcx, ARRAY_SIZE
  mov       al, byte 1
  lea       rdi, [rel candidate_array]
  rep       stosb                   ; Copy rcx copies of al to rdi.
  ; r13 refers to the end of the array so that loop termination can just check
  ; against 0 instead of doing an explicit comparision.
  lea       r13, [rel candidate_array_end]
  mov       [r13], byte 1            ; Sentinal value so that candidate loops don't need to check for ARRAY_SIZE

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
  mov       [rsp+WHEEL_LEN_OFFSET], rbx

  ; Determine the parameters for updating the wheel offset.
  xor       rdx, rdx                ; |
  mov       rax, ARRAY_SIZE         ; |
  idiv      rbx                     ; | d = ARRAY_SIZE%w
  mov       rax, rbx                ; |
  sub       rax, rdx                ; | a = w - ARRAY_SIZE%w

  ; The current offset of the wheel.
  mov       [rsp+WHEEL_OFFSET_OFFSET], dword 0
  ; How much we need to decrement by to update the offset for the next segment.
  ; We choose decrement, as we can correct by checking if the value is negative.
  mov       [rsp+WHEEL_DEC_OFFSET], eax

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
  mov       rax, [rsp+WHEEL_LEN_OFFSET] ; |
  shr       rax, 3                      ; |
  add       rax, ARRAY_SIZE/8+1         ; | a = (w+ARRAY_SIZE)/8+1
  memcpy    [rel template_candidate_array], \
            [rel candidate_array], \
            rax

  ; Increment the offset for alignment with the wheel.
  xor       eax, eax                       ; |
  mov       edx, [rsp+WHEEL_OFFSET_OFFSET] ; |
  mov       ecx, [rsp+WHEEL_DEC_OFFSET]    ; |
  sub       edx, ecx                       ; | offset -= dec
  cmovl     rax, [rsp+WHEEL_LEN_OFFSET]    ; |
  add       edx, eax                       ; | if (offset < 0) offset += w
  mov       [rsp+WHEEL_OFFSET_OFFSET], edx ; |

  ; Align candidate_array with the wheel.
  ; (we have padding at the end so it's ok to go over).
  lea       r13, [rel candidate_array_end]
  add       r13, rdx
  mov       [r13], byte 1            ; Sentinal value so that candidate loops don't need to check for ARRAY_SIZE

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
  mov       r8, [rsp+OUTPUT_LEN_OFFSET]
  mov       r9, [rsp+NEXT_POW_OFFSET]
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
  lea       rsi, [rsi+r8-1]         ; | buf += output_len - 1
  ; The start of the loop is copied here to reduce branching.
  ; However, this means that single digit numbers will have a zero in front.
  mov       rax, 0xcccccccccccccccd ; | a = ceil(2**64 * 8 / 10)
  mul       r12                     ; | d:a = a * b
  shr       rdx, 3                  ; | d = (d:a)/2**64/8 = b/10
print_segment_itoa_loop:
  ; Convert the last digit of b (r12) to a character (note: '0' = 48).
  lea       rcx, [rdx+rdx*4-24]     ; c = a*5 - 24
  shl       rcx, 1                  ; c *= 2 (c = a*10 - 48)
  sub       r12, rcx                ; b -= c (b = b - (b/10)*10 + 48 = b%10 + 48)
  mov       [rsi], r12b             ; *buf = b (add char to buffer)
  sub       rsi, 1                  ; buf--
  mov       r12, rdx                ; b = d (b = b'/10)
  mov       rax, 0xcccccccccccccccd ; | a = ceil(2**64 * 8 / 10)
  mul       r12                     ; | d:a = a * b
  shr       rdx, 3                  ; | d = (d:a)/2**64/8 = b/10
  jnz       print_segment_itoa_loop
  ; Handle the final digit
  or        r12d, '0'               ; |
  mov       [rsi], r12b             ; | *buf = b%10 + 48 (add char to buffer)
  ; Finalize
  mov       [rsi+r8], byte `\n`     ; | *(buf+output_len) = '\n'
  lea       rsi, [rsi+r8+1]         ; | buf += output_len + 1
  jmp       print_segment_loop
print_segment_write:
  ; Restore local variables.
  mov       [rsp+OUTPUT_LEN_OFFSET], r8
  mov       [rsp+NEXT_POW_OFFSET], r9
  ; If we have nothing to write, don't call _puts because it adds a newline.
  cmp       rsi, rdi
  je        print_segment_skip
  ; Overwrite the last newline with a null byte to terminate the string.
  mov       [rsi-1], byte 0
  call _puts
print_segment_skip:

; Continue looping until we reach the search limit.
  add       rbx, ARRAY_SIZE
  cmp       rbx, [rsp+SEARCH_LIMIT_OFFSET]
  jl        all_segments_loop

exit:
  add       rsp, LOCAL_VAR_BYTES
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

print_buffer:
  ; 20 bytes is enough to store 2**64
  resb ARRAY_SIZE*20