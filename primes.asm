; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

; SEGMENT_SIZE equ 10_000
SEGMENT_SIZE   equ 10
MAX_PRIME    equ SEGMENT_SIZE*SEGMENT_SIZE
ARRAY_SIZE   equ SEGMENT_SIZE/2

NEWLINE     equ 10 ; newline ascii character

global    _main
extern    _puts

section   .text

_main:
  push      rsp                     ; Required for alignment

initialize:
  mov       r12, 2                  ; p = 2
  call      print_r12
  mov       r12, 1                  ; p = 1
  lea       r13, [rel candidate_array]

; Set the candidate array back to all 1s
set_candidate_array:
  mov       rax, 1
set_candidate_array_loop:
  mov       [r13+rax], byte 1       ; candidate_array[a++] = 1
  inc       rax
  cmp       rax, ARRAY_SIZE
  jl        set_candidate_array_loop

; Find primes in initial segment.
  mov       r14, 0                  ; x = 0

; Find the next prime to clear in the initial segment.
find_next_p:
  inc       r14                     ; x++
  add       r12, 2                  ; p += 2
  cmp       [r13+r14], byte 0
  je        find_next_p             ; if (candidate_array[x] == 0) find_next_p

  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rax, r12                ; a = p
  mul       rax                     ; a = a*a
  ; If p*p is past the end of the array, we found all the primes in the segment.
  cmp       rax, SEGMENT_SIZE
  jge       collect_initial_primes  ; | if (a >= ARRAY_SIZE) collect_initial_primes
  ; Shift a to account for the fact that the array only contains odd numbers.
  shr       rax, 1                  ; a = a/2 = (p*p)/2

; Clear k*p from the initial segment.
clear_initial_prime_multiples:
  mov       [r13+rax], byte 0       ; candidate_array[a] = 0
  add       rax, r12                ; a += p
  cmp       rax, ARRAY_SIZE         ; if (a >= ARRAY_SIZE) find_next_p
  jge       find_next_p
  jmp       clear_initial_prime_multiples

; Collect and print the primes in the initial segment.
collect_initial_primes:
  mov       r14, 1                  ; x = 1
  mov       r15, 0                  ; n = 0
  mov       r12, 3                  ; p = 3

collect_initial_primes_loop:
  cmp       [r13+r14], byte 0
  je        collect_initial_primes_next ; | if (candidate_array[x] == 0) collect_initial_primes_next
collect_initial_primes_found:
  lea       r11, [rel initial_primes]
  mov       [r11+r15*8], r12        ; initial_primes[n] = p
  inc       r15                     ; n++
  call print_r12
collect_initial_primes_next:
  inc       r14                     ; x++
  add       r12, 2                  ; p += 2
  cmp       r12, SEGMENT_SIZE           ; |
  jl        collect_initial_primes_loop ; | if (p < SEGMENT_SIZE) collect_initial_prime_loop

rest_segments:
  mov       rbx, 1
rest_segments_loop:
  cmp       rbx, SEGMENT_SIZE
  jge       exit

; r15: n = number of primes
; r11: initial_primes
; Set the candidate array back to all 1s
reset_candidate_array:
  mov       rax, 0
reset_candidate_array_loop:
  mov       [r13+rax], byte 1       ; candidate_array[a++] = 1
  inc       rax
  cmp       rax, ARRAY_SIZE
  jl        reset_candidate_array_loop

handle_segment:
  mov       r14, 0                  ; x = 0
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  lea       r11, [rel initial_primes]
  mov       r12, [r11+r14*8]        ; p = initial_primes[x]
  inc       r14                     ; x++
  mov       rax, SEGMENT_SIZE
  mul       rbx                     ; a = SEGMENT_SIZE * b
  xor       rdx, rdx
  idiv      r12                     ; d = a%p
  test      rax, 1
  jnz       odd
  mov       rax, r12
  sub       rax, rdx                ; a = p-a%p
  shr       rax, 1
  jmp clear_prime_multiples
odd:
  mov       rax, r12
  add       rax, rax
  sub       rax, rdx                ; a = p-a%p
  shr       rax, 1

clear_prime_multiples:
  cmp       rax, ARRAY_SIZE         ; if (a >= ARRAY_SIZE) handle_segment_loop
  jge       handle_segment_loop
  mov       [r13+rax], byte 0       ; candidate_array[a] = 0
  add       rax, r12                ; a += p
  jmp       clear_prime_multiples

; Print the primes in the current segment segment.
print_segment:
  mov       r14, 0                  ; x = 0
print_segment_loop:
  cmp       [r13+r14], byte 0
  je        print_segment_next ; | if (candidate_array[x] == 0) print_segment_next
print_segment_found:
  mov       rax, SEGMENT_SIZE
  mul       rbx
  mov       r12, rax
  lea       r12, [r12+r14*2+1]
  call print_r12
print_segment_next:
  inc       r14                     ; x++
  cmp       r14, ARRAY_SIZE         ; |
  jl        print_segment_loop ; | if (p < SEGMENT_SIZE) print_segment_next

  inc rbx
  jmp rest_segments_loop

exit:
  pop       rsp                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

; Print out the number at r12.
print_r12:
  lea       rdi, [rel print_buffer_end] ; buf = print_buffer_end
  mov       r11, r12                ; b = p
print_r12_loop:
  mov       rax, 0xcccccccccccccccd ; | a = ceil(2**64 * 8 / 10)
  mul       r11                     ; | d:a = a * b
  shr       rdx, 3                  ; | d = (d:a)/2**64/8 = b/10
  lea       rcx, [rdx+rdx*4-24]     ; c = a*5 - 24 (the 24 will convert the number to ascii).
  add       rcx, rcx                ; c *= 2 (c = a*10 - 48)
  sub       r11, rcx                ; b -= c (b = b - (b/10)*10 + 48 = b%10 + 48)
  dec       rdi                     ; buf--
  mov       [rdi], r11b             ; *buf = b (add char to buffer)
  mov       r11, rdx                ; b = d (b = b'/10)
  test      rdx, rdx                ; if d != 0: continue
  jnz       print_r12_loop
print_r12_finish:
  push      rsp                     ; Required for alignment.
  call _puts
  pop       rsp
  ret

print_sep:
  push      rsp                     ; Required for alignment.
  lea       rdi, [rel sep]
  call _puts
  pop       rsp
  ret

section   .data

print_buffer:
  times 20 db 0                     ; Enough space to store 2**64
print_buffer_end:
  db 0
sep:
  db "----", 0

section .bss

candidate_array:
  resb ARRAY_SIZE

  alignb 16
initial_primes:
  ; TODO: Can these just be 32 bit?
  resq ARRAY_SIZE
