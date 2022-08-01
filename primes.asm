; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

MAX_P        equ 100_000_000
; MAX_P   equ 100
ARRAY_SIZE   equ MAX_P/2 + 1

NEWLINE     equ 10 ; newline ascii character

global    _main
extern    _puts

section   .text

_main:
  push      rsp                     ; Required for alignment

  mov       r12, 2                  ; p = 2
  call      print_r12
  mov       r14, 0                  ; x = 0
  mov       r12, 1                  ; p = 1
  lea       r13, [rel prime_array]  ; q = prime_array

  mov       rax, 1
set_prime_array:
  mov       [r13+rax], byte 1       ; q[a++] = 1
  inc       rax
  cmp       rax, ARRAY_SIZE
  jl        set_prime_array

find_next_p:
  xor       eax, eax                ; Must clear because we only read bytes into al
find_next_p_loop:
  inc       r14                     ; x++
  add       r12, 2                  ; p += 2
  cmp       r12, MAX_P              ; if (p > MAX_P) exit
  jg        exit
  mov       al, [r13+r14]           ; if (q[x] == 0) find_next_p
  test      eax, eax
  jz        find_next_p_loop

  call      print_r12               ; p (r12) is a prime, print it out.

  mov       rax, r12                ; a = (p*p)/2
  mul       rax                     ; We want to start from p*p, but we shift a
  shr       rax, 1                  ; because the array skips even numbers.

clear_prime_multiples:
  cmp       rax, ARRAY_SIZE         ; if (a >= ARRAY_SIZE) find_next_p
  jge       find_next_p
  mov       [r13+rax], byte 0       ; q[a] = 0
  add       rax, r12                ; a += p
  jmp       clear_prime_multiples

exit:
  pop       rsp                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

; Print out the number at r12.
print_r12:
  lea       rdi, [rel print_buffer_end] ; buf = print_buffer_end
  mov       rbx, r12                ; b = p
print_r12_loop:
  mov       rax, 0xcccccccccccccccd ; | a = ceil(2**64 * 8 / 10)
  mul       rbx                     ; | d:a = a * b
  shr       rdx, 3                  ; | d = (d:a)/2**64/8 = b/10
  lea       rcx, [rdx+rdx*4-24]     ; c = a*5 - 24 (the 24 will convert the number to ascii).
  add       rcx, rcx                ; c *= 2 (c = a*10 - 48)
  sub       rbx, rcx                ; b -= c (b = b - (b/10)*10 + 48 = b%10 + 48)
  dec       rdi                     ; buf--
  mov       [rdi], bl               ; *buf = b (add char to buffer)
  mov       rbx, rdx
  test      rdx, rdx                ; if d != 0: continue
  jnz       print_r12_loop
print_r12_finish:
  push      rsp                     ; Required for alignment.
  call _puts
  pop       rsp
  ret

section   .data

print_buffer:
  times 20 db 0                     ; Enough space to store 2**64
print_buffer_end:
  db 0

section .bss

prime_array:                        ; prime_array has MAX_P+1 elements
  resb ARRAY_SIZE
