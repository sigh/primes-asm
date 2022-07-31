; Run with:
;   nasm -fmacho64 hello.asm && gcc hello.o && ./a.out

MAX_P  equ 10_000_000

global    _main
extern    _printf

section   .text

_main:
  push      rsp                     ; Required for alignment

  mov r12, 2
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
  push rsp                          ; Required for alignment.
  lea       rdi, [rel format]
  mov       rsi, r12
  call      _printf
  pop rsp
  ret

section   .data

format:
  db        "%d", 10, 0             ; Printf format string, followed by newline and null