; Run with:
;   nasm -fmacho64 hello.asm && gcc hello.o && ./a.out

MAX_P   equ 10_000_000
; MAX_P   equ 100

NEWLINE     equ 10 ; newline ascii character

global    _main
extern    _puts

section   .text

_main:
  push      rsp                     ; Required for alignment

  mov       r12, 1                  ; p = 1
  lea       r13, [rel prime_array]  ; q = prime_array

find_next_p:
  xor       eax, eax                ; Must clear because we only read bytes into al
find_next_p_loop:
  inc       r12                     ; p++;
  cmp       r12, MAX_P              ; if (p > MAX_P) exit
  jg        exit
  mov       al, [r13+r12]           ; if (q[p] == 0) find_next_p
  test      eax, eax
  jz        find_next_p_loop

  call      print_r12               ; p (r12) is a prime, print it out.

  mov       rax, r12                ; a = p*p
  mul       rax

clear_prime_multiples:
  cmp       rax, MAX_P              ; if (a > MAX_P) continue
  jg        find_next_p
  mov       [r13+rax], byte 0       ; q[a] = 0
  add       rax, r12                ; a += p
  jmp       clear_prime_multiples

exit:
  pop       rsp                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

; Print out the number at r12.
print_r12:
  push      rsp                     ; Required for alignment.
  lea       rdi, [rel print_buffer_end] ; buf = print_buffer_end
  mov       rax, r12                ; a = p
  mov       rbx, 10                 ; b = 10
print_r12_loop:
  xor       rdx, rdx                ; clear d
  idiv      rbx                     ; a, d = divmod(a, b)
  lea       rdx, [rdx + 48]         ; d += 48 (convert digit to ascii)
  dec       rdi                     ; buf--
  mov       [rdi], dl               ; *buf = d (add char to buffer)
  test      rax, rax                ; if a != 0: continue
  jnz       print_r12_loop
print_r12_finish:
  call _puts
  pop       rsp
  ret

section   .data

print_buffer:
  times 20 db 0                     ; Enough space to store 2**64
print_buffer_end:
  db 0

prime_array:
  db 0, 0
  times MAX_P db 1
