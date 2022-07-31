; Run with:
;   nasm -fmacho64 hello.asm && gcc hello.o && ./a.out

global    _main
extern    _puts
extern    _printf

section   .text

_main:
  push      rbx                     ; Call stack must be aligned

  mov       r12, $47                 ; x = p
  mov       r13d, $2                 ; i = 2
  mov       r14, $1                  ; is_prime = true
is_prime_loop:
  lea       rdi, [rel format]       ; First argument: format string
  mov       rsi, r13                 ; Second argument: number to formart
  call      _printf

  mov       eax, r13d
  mul       eax                     ; a = i*i
  cmp       rax, r12                ; if (a > p) done
  jg        done
  xor       rdx, rdx
  mov       rax, r12
  idiv      r13                     ; d = p % i
  test      rdx, rdx
  jz        not_prime               ; if (d == 0) goto not_prime
  inc       r13d                    ; i++
  jmp is_prime_loop

not_prime:
  mov       r14, $0

done:

  lea       rdi, [rel format]       ; First argument: format string
  mov       rsi, r14                ; Second argument: number to formart
  call      _printf

  pop       rbx                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

section   .data

format:
  db        "%d", 10, 0             ; Printf format string, followed by newline and null