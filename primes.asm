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
WHEEL_SIZE    equ 30

LOCAL_VAR_BYTES     equ 32
SEARCH_LIMIT_OFFSET equ 8
OUTPUT_LEN_OFFSET   equ 24
NEXT_POW_OFFSET     equ 16

; debug <format> <value>
; Save a bunch of callee saved registers for convinience.
%macro debug 2
  ; Push the things we want to save.
  push      r11
  push      r12
  push      rax
  push      rcx
  push      rdi
  push      rsi
  ; Do the print
  mov       rsi, %2
  lea       rdi, [rel %1]
  call _printf
  ; Pop everything.
  pop       rsi
  pop       rdi
  pop       rcx
  pop       rax
  pop       r12
  pop       r11
%endmacro

; roll_wheel <label> <offset>
%macro roll_wheel 2
  ; Align wheel with offset (we have padding in candidate array so it's ok to
  ; overwrite negative entries).
  mov       rdi, %2                 ; |
  mov       rax, 0x8888888888888889 ; |
  mul       rdi                     ; |
  shr       rdx, 3                  ; | d = offset/15 (15 = WHEEL_SIZE/2)
  mov       rcx, rdx                ; |
  sal       rcx, 4                  ; | c = d*16
  lea       rdx, [rdi + rdx + ARRAY_SIZE] ;
  sub       rcx, rdx                ; | c = -offset%15-ARRAY_SIZE
                                    ; |   = (d*16-d)       - offset - ARRAY_SIZE
                                    ; |   = (offset/15)*15 - offset - ARRAY_SIZE
  ; Roll wheel 16 entries at a time.
  mov       rax, 0x0001010001000001
  mov       rdx, 0x0101000001000101
  ; Index relative to end of array so that the loop condition is checked by the add.
  lea       rdi, [rel candidate_array_end]
roll_wheel_loop_%1:
  mov       [rdi+rcx], rax
  mov       [rdi+rcx+8], rdx
  add       rcx, WHEEL_SIZE/2
  jl        roll_wheel_loop_%1
%endmacro

section   .text

_main:
  push      rsp                     ; Required for alignment
  sub       rsp, LOCAL_VAR_BYTES

initialize:
  ; Write out the first prime directly, as we elide all even numbers in the rest
  ; of the program.
  lea       rdi, [rel prelude]
  call      _puts

  ; Initialize variables.
  mov       r12, 1                  ; p = 1
  xor       r15, r15                ; n = 0
  xor       r14, r14                ; x = 0
  lea       r11, [rel initial_primes]
  mov       rax, 1
  mov       [rsp+OUTPUT_LEN_OFFSET], rax
  mov       rax, 10
  mov       [rsp+NEXT_POW_OFFSET], rax

  ; Set up the search limit
  mov       rax, SEARCH_LIMIT/2
  mov       [rsp+SEARCH_LIMIT_OFFSET], rax

  ; Set the candidate array to all 1s (except for 1).
  ; r13 refers to the end of the array so that loop termination can just check
  ; against 0 instead of doing an explicit comparision.
  lea       r13, [rel candidate_array_end]
  roll_wheel initial_primes, 0
  mov       [r13-ARRAY_SIZE], byte 0 ; candidate_array[0] = 0 (i.e. 1 is not a prime)
  mov       [r13], byte 1            ; Sentinal value so that candidate loops don't need to check for ARRAY_SIZE

; Find primes in initial segment.
collect_initial_primes:
  add       r14, 1                  ; x++
  add       r12, 2                  ; p += 2
  cmp       [r13+r14-ARRAY_SIZE], byte 0
  je        collect_initial_primes  ; if (candidate_array[x] == 0) collect_initial_primes

  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rax, r12                ; a = p
  mul       rax                     ; a = a*a
  ; Shift a to account for the fact that the array only contains odd numbers.
  shr       rax, 1                  ; a = a/2 = (p*p)/2
  sub       rax, ARRAY_SIZE
  ; If p*p is past the end of the array, we have marked all the primes in the segment.
  ; So we can just directly collect the rest.
  jge       collect_large_initial_primes  ; | if (a >= ARRAY_SIZE) collect_large_initial_primes

; Clear values f*p where f is even.
; This clears entries a+m*p from the candidate_array at r13.
; Where a = rax, p = r12
%macro clear_prime_multiples 1
clear_prime_multiples_%1:
  mov       [r13+rax], byte 0       ; candidate_array[a] = 0
  add       rax, r12                ; a += p
  jl        clear_prime_multiples_%1 ; if (a < 0) continue
%endmacro

  clear_prime_multiples initial_primes

  add       rax, ARRAY_SIZE         ; |
  mov       [r11+r15], rax          ; |
  mov       [r11+r15+8], r12d       ; | initial_primes[n/16] = (f/2, p)
  add       r15, 16                 ; n += 16
  jmp       collect_initial_primes

; Collect the rest of the primes in the initial segment.
; These primes are too large to affect the first segment.
collect_large_initial_primes:
  sub       r14, 1
collect_large_initial_primes_loop:
  ; NOTE: Because we have a sentinal element, we don't need to check the loop bounds here.
  add       r14, 1
  cmp       [r13+r14-ARRAY_SIZE], byte 0
  je        collect_large_initial_primes_loop ; if (candidate_array[x] == 0) collect_large_initial_primes_loop
collect_large_initial_primes_found:
  cmp       r14, ARRAY_SIZE         ; |
  jge       all_segments            ; | if (x >= ARRAY_SIZE) goto all_segments
  lea       r12, [r14*2+1]          ; p = x*2 + 1
  mov       rax, r12                ; |
  mul       rax                     ; | f = p*p (next factor to look at)
  shr       rax, 1                  ; |
  mov       [r11+r15], rax          ; |
  mov       [r11+r15+8], r12d       ; | initial_primes[n/16] = (f/2, p)
  add       r15, 16                 ; n += 16
  jmp       collect_large_initial_primes_loop

; Now that we've found the initial primes, iterate over all segments find the
; rest.
all_segments:
  xor       rbx, rbx                ; segment_start = 0
  ; We want to use print_segment for the first segment as well.
  ; However, it doesn't handle single digit output so we do those manually and
  ; cross them off the list here.
  mov       [r13-ARRAY_SIZE], dword 0
  jmp       print_segment

all_segments_loop:

; Find the primes in the next segment by sieving out all of the initial primes.
handle_segment:
  roll_wheel segment, rbx
  xor       r14, r14                ; x = 0
  lea       r11, [rel initial_primes]
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  mov       rax, [r11+r14]          ; |
  mov       r12d, [r11+r14+8]       ; | (p, f/2) = initial_primes[x/16]
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

  resb WHEEL_SIZE/2  ; buffer
candidate_array:
  resb ARRAY_SIZE
candidate_array_end:
  resb WHEEL_SIZE/2  ; buffer

  alignb 16
initial_primes:
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