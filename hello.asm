; Run with:
;   nasm -fmacho64 hello.asm && gcc hello.o && ./a.out

MAX_P   equ 10_000_000
; MAX_P   equ 10

NEWLINE     equ 10 ; newline ascii character

global    _main
extern    _puts

section   .text

_main:
  push      rsp                     ; Required for alignment

  mov       r12, 2
  call      print_r12

  mov       r12, 3                  ; p = 3

iteration_loop:
  mov       r13d, 3                 ; i = 3

is_prime_loop:
  mov       eax, r13d
  mul       eax                     ; a = i*i
  cmp       rax, r12                ; if (a > p) done
  jg        is_prime
  xor       rdx, rdx
  mov       rax, r12
  idiv      r13                     ; d = p % i
  test      rdx, rdx
  jz        done                    ; if (d == 0) goto done
  lea       r13, [r13+2]            ; i += 2
  jmp is_prime_loop

is_prime:
  call print_r12

done:
  lea       r12, [r12+2]            ; p += 2
  cmp       r12, MAX_P
  jl        iteration_loop

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
