; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

%ifndef SQRT_SIZE
%define SQRT_SIZE 10_000
%endif
SEGMENT_SIZE  equ SQRT_SIZE
SEARCH_LIMIT  equ SEGMENT_SIZE*SEGMENT_SIZE
ARRAY_SIZE    equ SEGMENT_SIZE/2

LOCAL_VAR_BYTES     equ 16
SEARCH_LIMIT_OFFSET equ 8

; Set the candidate array back to all 1s
%macro reset_candidate_array 0
  mov       rcx, ARRAY_SIZE
  mov       al, byte 1
  lea       rdi, [rel candidate_array]
  rep       stosb                   ; Copy rcx copies of al to rdi.
%endmacro

; Convert a number to string.
; The number is genrated backwards, then reversed.
; itoa <loop_label> <i> <start_ptr>
%macro itoa 3
  ; Copy number to a temp register so that we don't change it.
  mov       r11, %2                 ; b = i
  ; Copy pointer to a temp register so that we can reuse it.
  mov       r10, %3
  ; The start of the loop is copied here to reduce branching.
  ; However, this means that single digit numbers will have a zero in front.
  mov       rax, 0xcccccccccccccccd ; | a = ceil(2**64 * 8 / 10)
  mul       r11                     ; | d:a = a * b
  shr       rdx, 3                  ; | d = (d:a)/2**64/8 = b/10
%1#_loop:
  ; Convert the last digit of r11 to a character (note: '0' = 48).
  lea       rcx, [rdx+rdx*4-24]     ; c = a*5 - 24
  shl       rcx, 1                  ; c *= 2 (c = a*10 - 48)
  sub       r11, rcx                ; b -= c (b = b - (b/10)*10 + 48 = b%10 + 48)
  mov       [%3], r11b              ; *buf = b (add char to buffer)
  add       %3, 1                   ; buf++
  mov       r11, rdx                ; b = d (b = b'/10)
  mov       rax, 0xcccccccccccccccd ; | a = ceil(2**64 * 8 / 10)
  mul       r11                     ; | d:a = a * b
  shr       rdx, 3                  ; | d = (d:a)/2**64/8 = b/10
  jnz       %1#_loop

  ; Handle the final digit
  or        r11d, '0'               ; |
  mov       [%3], r11b              ; | *buf = b%10 + 48 (add char to buffer)

  ; This can be reduced to two instructions but it hurts the runtime a lot for
  ; some reason.
  add       %3, 1                   ; buf++
  mov       r11, %3
  sub       r11, 1
%1#_reverse:
  mov       al, [r10]
  mov       cl, [r11]
  mov       [r11], al
  mov       [r10], cl
  add       r10, 1
  sub       r11, 1
  cmp       r10, r11
  jl        %1#_reverse
%endmacro

global    _main
extern    _puts

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

  ; Set up the search limit
  mov       rax, SEARCH_LIMIT/2
  mov       [rsp+SEARCH_LIMIT_OFFSET], rax

  ; Set the candidate array to all 1s (except for 1).
  ; r13 refers to the end of the array so that loop termination can just check
  ; against 0 instead of doing an explicit comparision.
  lea       r13, [rel candidate_array_end]
  reset_candidate_array
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
  reset_candidate_array
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
  lea       r12, [r12+r12-1]        ; | r12 = (segment_start + x)*2 + 1
  itoa      itoa_print_segment, r12, rsi
  mov       [rsi], byte `\n`        ; |
  add       rsi, 1                  ; | *buf++ = '\n'
  jmp       print_segment_loop
print_segment_write:
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

; Print out the number at rdi for debugging.
; Save a bunch of callee saved registers for convinience.
debug_u64:
  ; Push the things we want to save.
  push      r11
  push      r12
  push      rax
  push      rcx
  ; Convert the number to a string.
  mov       r12, rdi
  lea       rdi, [rel print_buffer]
  itoa      print_u64_itoa, r12, rdi
  mov       [rdi], byte 0
  lea       rdi, [rel print_buffer]
  ; Write.
  push      rsp                     ; Required for alignment.
  call _puts
  pop       rsp
  ; Pop everything.
  pop       rcx
  pop       rax
  pop       r12
  pop       r11
  ret

print_sep:
  push      rsp                     ; Required for alignment.
  lea       rdi, [rel sep]
  call _puts
  pop       rsp
  ret

section   .data

sep:
  db "----", 0
prelude:
  db `2\n3\n5\n7`, 0

section .bss

candidate_array:
  resb ARRAY_SIZE
candidate_array_end:
  resq 1  ; buffer

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