; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

; SEGMENT_SIZE equ 10_000
SEGMENT_SIZE   equ 10
MAX_PRIME    equ SEGMENT_SIZE*SEGMENT_SIZE
ARRAY_SIZE   equ SEGMENT_SIZE/2

NEWLINE     equ 10 ; newline ascii character

; Set the candidate array back to all 1s
%macro reset_candidate_array 0
  mov       rcx, ARRAY_SIZE
  mov       al, byte 1
  lea       rdi, [rel candidate_array]
  rep       stosb                   ; Copy rcx copies of al to rdi.
%endmacro

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
reset_candidate_array
; Find primes in initial segment.
  mov       [r13], byte 0           ; candidate_array[0] = 0 (i.e. 1 is not a prime)
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

; Clear rax+k*r12 from the candidate_array at r13.
%macro clear_prime_multiples 1
clear_prime_multiples_%1:
  cmp       rax, ARRAY_SIZE         ; if (a >= ARRAY_SIZE) goto %1
  jge       %1
  mov       [r13+rax], byte 0       ; candidate_array[a] = 0
  add       rax, r12                ; a += p
  jmp       clear_prime_multiples_%1
%endmacro

clear_prime_multiples find_next_p

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

reset_candidate_array

handle_segment:
  mov       r14, 0                  ; x = 0
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  lea       r11, [rel initial_primes]
  mov       r12, [r11+r14*8]        ; p = initial_primes[x]
  inc       r14                     ; x++

  ; Find the first ODD multiple of p in the current segment.
  ; Note that we only need to find the offset into the segment.
  mov       rax, SEGMENT_SIZE       ; |
  mul       rbx                     ; | a = SEGMENT_SIZE * b
  xor       rdx, rdx
  idiv      r12                     ; d = a%p, a = a/p
  and       al, 1                   ; |
  mov       cl, al                  ; | c == 1 if a is an odd multiple
  mov       rax, r12                ; |
  shl       rax, cl                 ; | a = c ? p*2 : p
  sub       rax, rdx                ; a -= (SEGMENT_SIZE*b)%p
  ; Half the offset to get to the offset into the array (as the array skips even numbers).
  shr       rax, 1

clear_prime_multiples handle_segment_loop

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