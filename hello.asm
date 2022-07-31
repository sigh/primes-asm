; Run with:
;   nasm -fmacho64 hello.asm && gcc hello.o && ./a.out

MAX_P  equ 10_000_000

global    _main
extern    _printf

section   .text

_main:
  push      rbx                     ; Call stack must be aligned

  lea       rdi, [rel format]       ; Print the first prime (2)
  mov       rsi, 2
  call      _printf

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
  lea       rdi, [rel format]       ; Print this prime (r12)
  mov       rsi, r12
  call      _printf

done:
  lea       r12, [r12+2]            ; p += 2
  cmp       r12, MAX_P
  jl        iteration_loop

exit:
  pop       rbx                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

section   .data

format:
  db        "%d", 10, 0             ; Printf format string, followed by newline and null