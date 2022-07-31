; Run with:
;   nasm -fmacho64 hello.asm && gcc hello.o && ./a.out

MAX_P  equ 100

global    _main
extern    _printf

section   .text

_main:
  push      rbx                     ; Call stack must be aligned

  mov       r12, 2                  ; p = 2

iteration_loop:
  mov       r13d, 2                 ; i = 2

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
  inc       r13d                    ; i++
  jmp is_prime_loop

is_prime:
  lea       rdi, [rel format]       ; First argument: format string
  mov       rsi, r12                 ; Second argument: number to formart
  call      _printf

done:
  inc r12
  cmp r12, MAX_P
  jne iteration_loop

exit:
  pop       rbx                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

section   .data

format:
  db        "%d", 10, 0             ; Printf format string, followed by newline and null