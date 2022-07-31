; Run with:
;   nasm -fmacho64 hello.asm && gcc hello.o && ./a.out

global    _main
extern    _puts
extern    _printf

section   .text

_main:
  push      rbx                     ; Call stack must be aligned

  lea       rdi, [rel message]      ; First argument is address of message
  call      _puts                   ; puts(message)

  lea       rdi, [rel format]       ; First argument: format string
  mov       rsi, $1                 ; Second argument: number to formart
  call      _printf

  pop       rbx                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

section   .data

message:
  db        "Hello world", 0
format:
  db        "%d", 10, 0