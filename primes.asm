; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

%ifndef SIZE
%define SIZE 10_000
%endif
SEGMENT_SIZE  equ SIZE
SEARCH_LIMIT  equ SEGMENT_SIZE*SEGMENT_SIZE
ARRAY_SIZE    equ SEGMENT_SIZE/2

NEWLINE     equ 10 ; newline ascii character

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
%1#_loop:
  mov       rax, 0xcccccccccccccccd ; | a = ceil(2**64 * 8 / 10)
  mul       r11                     ; | d:a = a * b
  shr       rdx, 3                  ; | d = (d:a)/2**64/8 = b/10
  ; Convert the last digit of r11 to a character (note: '0' = 48).
  lea       rcx, [rdx+rdx*4-24]     ; c = a*5 - 24
  shl       rcx, 1                  ; c *= 2 (c = a*10 - 48)
  sub       r11, rcx                ; b -= c (b = b - (b/10)*10 + 48 = b%10 + 48)
  mov       [%3], r11b              ; *buf = b (add char to buffer)

  add       %3, 1                   ; buf++
  mov       r11, rdx                ; b = d (b = b'/10)
  test      rdx, rdx                ; if d != 0: continue
  jnz       %1#_loop

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

  ; Set the candidate array to all 1s (except for 1).
  lea       r13, [rel candidate_array]
  reset_candidate_array
  mov       [r13], byte 0           ; candidate_array[0] = 0 (i.e. 1 is not a prime)

; Find primes in initial segment.
collect_initial_primes:
  add       r14, 1                  ; x++
  add       r12, 2                  ; p += 2
  cmp       [r13+r14], byte 0
  je        collect_initial_primes  ; if (candidate_array[x] == 0) collect_initial_primes

  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rax, r12                ; a = p
  mul       rax                     ; a = a*a
  ; If p*p is past the end of the array, we have marked all the primes in the segment.
  ; So we can just directly collect the rest.
  cmp       rax, SEGMENT_SIZE
  jge       collect_large_initial_primes  ; | if (a >= ARRAY_SIZE) collect_large_initial_primes
  ; Shift a to account for the fact that the array only contains odd numbers.
  shr       rax, 1                  ; a = a/2 = (p*p)/2
  xor       ecx, ecx                ; k = 0

; Clear values (k+p)*p where k is even.
; This clears entries a+m*p from the candidate_array at r13.
; Where a = rax, k = ecx, p = r12
%macro clear_prime_multiples 1
clear_prime_multiples_%1:
  mov       [r13+rax], byte 0       ; candidate_array[a] = 0
  add       rax, r12                ; a += p
  add       ecx, 2                  ; k += 2
  cmp       rax, ARRAY_SIZE         ; if (a < ARRAY_SIZE) continue
  jl        clear_prime_multiples_%1
%endmacro

  clear_prime_multiples initial_primes

  mov       [r11+r15*8], r12d       ; |
  mov       [r11+r15*8+4], ecx      ; |
  add       r15, 1                  ; | initial_primes[n++] = (p, k)
  jmp       collect_initial_primes

; Collect the rest of the primes in the initial segment.
; These primes are too large to affect the first segment.
collect_large_initial_primes:
  sub       r14, 1
collect_large_initial_primes_loop:
  add       r14, 1                  ; |
  cmp       r14, ARRAY_SIZE         ; |
  jge       all_segments            ; | if (++x >= ARRAY_SIZE) goto all_segments
  cmp       [r13+r14], byte 0
  je        collect_large_initial_primes_loop ; | if (candidate_array[x] == 0) collect_large_initial_primes_loop
collect_large_initial_primes_found:
  lea       r12, [r14*2+1]          ; p = x*2 + 1
  mov       [r11+r15*8], r12d       ; |
  mov       [r11+r15*8+4], dword 0  ; |
  add       r15, 1                  ; | initial_primes[n++] = (p, 0)
  jmp       collect_large_initial_primes_loop

; Now that we've found the initial primes, iterate over all segments find the
; rest.
all_segments:
  xor       rbx, rbx                ; segment_start = 0
all_segments_loop:

; Print the primes in the current segment.
print_segment:
  xor       r14, r14                ; x = 0
  lea       rdi, [rel print_buffer] ; buf = print_buffer
print_segment_loop:
  cmp       [r13+r14], byte 0       ; |
  je        print_segment_next      ; | if (candidate_array[x] == 0) print_segment_next
print_segment_found:
  lea       r12, [rbx+r14*2+1]      ; | r12 = segment_start + x*2 + 1
  itoa      itoa_print_segment, r12, rdi
  mov       [rdi], byte NEWLINE     ; |
  add       rdi, 1                  ; | *buf++ = '\n'
print_segment_next:
  add       r14, 1                  ; |
  cmp       r14, ARRAY_SIZE         ; |
  jl        print_segment_loop      ; | if (++x < ARRAY_SIZE) print_segment_loop
print_segment_write:
  ; Overwrite the last newline with a null byte to terminate the string.
  mov       [rdi-1], byte 0
  lea       rdi, [rel print_buffer]
  call _puts

; Finish up if we have reached our max.
  add       rbx, SEGMENT_SIZE
  cmp       rbx, SEARCH_LIMIT
  jge       exit

; Find the primes in the next segment by sieving out all of the initial primes.
handle_segment:
  reset_candidate_array
  xor       r14, r14                ; x = 0
handle_segment_loop:
  cmp       r14, r15
  jge       all_segments_loop       ; if (x >= n) all_segments_loop
  lea       r11, [rel initial_primes]
  mov       r12d, [r11+r14*8]       ; |
  mov       eax, [r11+r14*8+4]      ; | (p, k) = initial_primes[x]
  mov       ecx, eax                ; save k save the updated value back
  add       r14, 1                  ; x++
  ; Find the next multiple for sieving
  add       eax, r12d               ; |
  mul       r12                     ; |
  sub       rax, rbx                ; | a = (k+p)*p - segment_start
  ; Check if this multiple is too large for the segment.
  cmp       rax, SEGMENT_SIZE
  jge       handle_segment_early_exit
  ; Half the offset to get to the offset into the array (as the array skips even numbers).
  shr       rax, 1

  clear_prime_multiples all_segments
  mov       [r11+r14*8-4], ecx      ; Save the updated value of k
  jmp       handle_segment_loop
handle_segment_early_exit:
  ; Check if we can exit early.
  ; If the next multiple `k` is 0, then it means that it's too early for this
  ; prime to have any effect. This means we can skip over all future primes also.
  cmp       [r11+r14*8-4], dword 0
  je        all_segments_loop
  jmp       handle_segment_loop

exit:
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
  db "2", 0

section .bss

candidate_array:
  resb ARRAY_SIZE

  alignb 16
initial_primes:
  ; TODO: Can these be packed further by storing deltas?
  ; Store pairs of 32-bit uints (p, k) where:
  ;  - p is the prime
  ;  - (k+p)*p is the next candidate to be removed
  resq ARRAY_SIZE

print_buffer:
  ; 20 bytes is enough to store 2**64
  resb ARRAY_SIZE*20