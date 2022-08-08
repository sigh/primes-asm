; Run with:
;   nasm -fmacho64 primes.asm && gcc primes.o && ./a.out

global    _main
extern    _puts
extern    _printf

%ifndef SEGMENT_HINT
%define SEGMENT_HINT 65535
%endif
%ifndef LIMIT
%define LIMIT SEGMENT_SIZE*SEGMENT_SIZE
%endif
; Ensure segments are 8 byte aligned.
SEGMENT_SIZE           equ ((SEGMENT_HINT/128)+1)*128
SEARCH_LIMIT_BITS      equ LIMIT/2
SEGMENT_SIZE_BITS      equ SEGMENT_SIZE/2
ARRAY_SIZE_BITS        equ SEGMENT_SIZE_BITS
ARRAY_SIZE_BYTES       equ ARRAY_SIZE_BITS/8
; The minimum SEGMENT_SIZE=128 which has density < 4.
MAX_PRIMES_PER_SEGMENT equ SEGMENT_SIZE/4
; This is enough for any 64-bit value.
; See https://en.wikipedia.org/wiki/Prime_gap for table.
MAX_PRIME_GAP          equ 1200

%if LIMIT > SEGMENT_SIZE*SEGMENT_SIZE
%error "Segment size is too small for search limit."
%endif

STACK_VAR_BYTES       equ 48
SEARCH_LIMIT_BITS_VAR equ 8
OUTPUT_LEN_VAR        equ 16
NEXT_POW_VAR          equ 24
WHEEL_SIZE_BITS_VAR   equ 32
WHEEL_DEC_BITS_VAR    equ 36
WHEEL_OFFSET_BITS_VAR equ 40
SIEVE_PRIME_BYTES_VAR equ 44

%macro xmm_save 0
  ; Set flags to save all registers.
  mov       rax, -1
  mov       rdx, -1
  xsave     [rel xmm_save_space]
%endmacro
%macro xmm_restore 0
  ; Set flags to restore all registers.
  mov       rax, -1
  mov       rdx, -1
  xrstor    [rel xmm_save_space]
%endmacro

; debug <format> <value>
; Save a bunch of callee saved registers for convinience.
%macro debug 2
  ; Push the things we want to save.
  push      r8
  push      r9
  push      r10
  push      r11
  push      rax
  push      rcx
  push      rdx
  push      rdi
  push      rsi
  push      rsi
  xmm_save
  ; Do the print
  mov       rsi, %2
  lea       rdi, [rel format_%1]
  call _printf
  ; Pop everything.
  xmm_restore
  pop       rsi
  pop       rsi
  pop       rdi
  pop       rdx
  pop       rcx
  pop       rax
  pop       r11
  pop       r10
  pop       r9
  pop       r8
%endmacro

; Copies data in 8 byte chunks.
; memcopy_q <src> <dest> <num quadwords>
%macro memcpy_q 3
  lea rsi, %1
  lea rdi, %2
  mov rcx, %3
  rep movsq
%endmacro

; Adds 2 Binary-Coded Decimal numbers:
;  dst += src
; scratch is used as a scratch register.
; bcd_add <dst> <src> <scratch>
%macro bcd_add 3
  ; TODO lea instead of double add.
  add       %1, %2
  mov       %3, 0xF6F6F6F6F6F6F6F6
  add       %1, %3
  mov       %2, %1
  mov       %3, 0x6060606060606060
  and       %2, %3
  shr       %2, 4
  mov       %3, 0x0F0F0F0F0F0F0F0F
  and       %1, %3
  sub       %1, %2
%endmacro

section   .text

_main:
  push      rsp                     ; Required for alignment
  sub       rsp, STACK_VAR_BYTES

build_bcd_lookup:
  lea       rdi, [rel bcd_lookup]
  xor       rcx, rcx
  xor       rax, rax
build_bcd_lookup_loop:
  mov       [rdi+rcx*4], eax
  mov       rbx, 1
  bcd_add   rax, rbx, rdx
  add       rcx, 1
  cmp       rcx, MAX_PRIME_GAP
  jl        build_bcd_lookup_loop

initialize:
  ; Write out the first primes directly.
  ;  - 2 has to be printed directly because the rest of the program assumes odd
  ;    numbers.
  ;  - The single digit primes are printed out as the itoa code doesn't handle
  ;    them.
  lea       rdi, [rel prelude]
%ifndef QUIET
  call      _puts
%endif

  ; Initialize stack variables.
  mov       rax, 1
  mov       [rsp+OUTPUT_LEN_VAR], rax
  mov       rax, 10
  mov       [rsp+NEXT_POW_VAR], rax
  mov       rax, SEARCH_LIMIT_BITS
  mov       [rsp+SEARCH_LIMIT_BITS_VAR], rax

  ; Initialize the initial_segment_array with 1s.
  mov       rcx, ARRAY_SIZE_BYTES
  mov       al, byte 0xFF
  lea       rdi, [rel initial_segment_array]
  rep       stosb                   ; Copy rcx copies of al to rdi.
  lea       r13, [rel initial_segment_array]
  ; Mark 1 as not a prime.
  and       [r13], byte 0xFE

  ; Initial variables used in the first segment.
  lea       r11, [rel sieve_primes]
  lea       r8, [rel segment_array]
  mov       r12, 1                  ; p = 1 (current prime)
  xor       r14, r14                ; x = 0 (byte offset into initial_segment_array)
  mov       rbx, 2                  ; w = 1 (wheel_size_bits)

; Find the primes to use for the wheel.
; Keep collecting primes until their product is too large for the first segment.
; NOTE: ALL wheel primes will be in the first quad (i.e. < 128) as a larger
;       wheel will be >10^48 (>2^160).
collect_wheel_primes:
  ; Find the LSB.
  bsf       rcx, [r13]
  ; We found a prime! Figure out the value.
  ; NOTE: Wait until confirming this prime is part of the wheel beforing
  ; unsetting it.
  lea       r12, [rcx+rcx+1]        ; | p = c*2+1
  ; Determine if we've found all the wheel primes.
  mov       rax, r12                ; |
  mul       rbx                     ; |
  cmp       rax, SEGMENT_SIZE_BITS  ; |
  jg        fill_template_array     ; | if (w*p > SEGMENT_SIZE_BITS)
                                    ; |   fill_template_array
  mov       rbx, rax                ; w = w*p
  ; Unset the LSB.
  mov       rdi, [r13]
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13], rax
  ; Move the prime into the segment array.
  xor       rax, rdi
  or        [r8], rax
  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rcx, r12                ; |
  imul      rcx, rcx                ; | c = p*p
  ; Shift c to account for the fact that the array only contains odd numbers.
  shr       rcx, 1                  ; | c /= 2

; Clear values f*p where f is even.
; This clears bits c+m*p from the segment_array at r13.
; Where c = rcx, p = r12
; clear_prime_multiples <label> <limit>
%macro clear_prime_multiples 2
  mov       rsi, -2
clear_prime_multiples_%1:
  mov       rdx, rsi
  mov       rax, rcx
  rol       dl, cl                  ; Important: register width MUST match shift amount.
  shr       rax, 3
  and       [r13+rax], dl           ; segment_array[c/8] &= ~(1<<(c&8))
  add       rcx, r12                ; a += p
  cmp       rcx, %2
  jl        clear_prime_multiples_%1 ; if (c < (limit)) continue
%endmacro

  clear_prime_multiples wheel_primes, SEGMENT_SIZE_BITS
  ; NOTE: We don't store wheel primes in sieve_primes, as they are automatically
  ;       excluded by the wheel.
  jmp collect_wheel_primes

fill_template_array:
  ; Mark 1 back in as a template candidate.
  or        [r13], byte 0x01
  ; Copy the wheel to the template.
  memcpy_q  [rel initial_segment_array], \
            [rel template_segment_array], \
            ARRAY_SIZE_BYTES/8
  ; Determine how offset the wheel is.
  xor       rdx, rdx                ; |
  mov       rax, SEGMENT_SIZE_BITS  ; |
  idiv      rbx                     ; | d = SEGMENT_SIZE_BITS%w
  ; Fill another extra rotation of the wheel to allow for any offset.
  mov       r9, rdx                 ; bit_offset = d
  lea       r10, [r9+rbx]           ; max_bit_offset = bit_offset + w
  lea       r14, [rel template_segment_array+ARRAY_SIZE_BYTES]
  ; We fill in 4 bytes at a time.
  ; Each iteration we read 8 bytes so that we always have slack to shift in from.
fill_template_array_end:
  mov       rax, r9                 ; | Get the dword containing the wheel position.
  shr       rax, 5                  ; | index = bit_offset/32
  mov       rax, [r13+rax*4]        ; |
  mov       rcx, r9                 ; |
  and       rcx, 31                 ; |
  shr       rax, cl                 ; | a = segment_array[index*4] >> bit_offset%32
  mov       [r14], eax              ; | Align and assign dword to the template.
                                    ; | We shifted with a 64-bit value, so this is safe.
  add       r14, 4                  ; Increment BYTE pointer
  add       r9, 32                  ; Increment BIT offset
  cmp       r9, r10                 ; if (bit_offset < max_bit_offset) continue
  jle       fill_template_array_end
  ; Remove 1 again from the candidates.
  and       [r13], byte 0xFE

  ; Determine the parameters for updating the wheel offset.
  mov       [rsp+WHEEL_OFFSET_BITS_VAR], dword 0
  mov       [rsp+WHEEL_SIZE_BITS_VAR], ebx
  ; How much we need to decrement by to update the offset for the next segment.
  ; We choose decrement, as we can correct by checking if the value is negative.
  sub       rbx, rdx                ; a = w - SEGMENT_SIZE_BITS%w
  mov       [rsp+WHEEL_DEC_BITS_VAR], ebx

; Find primes for sieving (in the first segment).
  xor       r15, r15                  ; | n = 0 (offset into sieve_primes)
  mov       r14, -8                   ; | x = -8 (byte index into initial_segment_array)
  xor       rdi, rdi
collect_sieve_primes_inc:
  ; Find the next non-zero quad.
  add       r14, 8
collect_sieve_primes:
  add       rdi, [r13+r14]
  je        collect_sieve_primes_inc
  ; Find the LSB.
  bsf       rcx, rdi
  ; We found a prime! Figure out the value.
  ; NOTE: Wait until confirming this prime is part of the wheel beforing
  ; unsetting it.
  mov       rax, r14                ; |
  shl       rax, 3                  ; | convert from byte count to bit count.
  add       rcx, rax                ; |
  lea       r12, [rcx+rcx+1]        ; | p = (c+x*8)*2+1
  ; Calculate a = p*p. This is the first candidate we want to start clearing,
  ; as all the previous candidates have been cleared already.
  mov       rcx, r12                ; |
  imul      rcx, rcx                ; | c = p*p
  cmp       rcx, SEGMENT_SIZE
  ; If p*p is not in the segment, we have marked all the primes in the segment.
  ; So we can just directly collect the rest.
  jge       collect_large_sieve_primes  ; | if (p*p >= SEGMENT_SIZE) collect_large_sieve_primes
  ; Unset the LSB.
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13+r14], rax
  ; Move the prime into the segment array.
  xor       rax, rdi
  or        [r8+r14], rax
  ; Clear rdi for the next iteration.
  xor       rdi, rdi
  ; Shift c to account for the fact that the array only contains odd numbers.
  shr       rcx, 1                  ; | c /= 2

  clear_prime_multiples sieve_primes, SEGMENT_SIZE_BITS

  mov       [r11+r15], rcx          ; |
  mov       [r11+r15+8], r12d       ; | sieve_primes[n/16] = (f/2, p)
  add       r15, 16                 ; n += 16
  jmp       collect_sieve_primes

; Collect the rest of the primes in the first segment.
; These primes are too large to affect the first segment.
collect_large_sieve_primes:
  xor       rdi, rdi
  jmp       collect_large_sieve_primes_loop
collect_large_sieve_primes_inc:
  ; Find the next non-zero quad.
  add       r14, 8
  cmp       r14, ARRAY_SIZE_BYTES
  jge       all_segments
collect_large_sieve_primes_loop:
  add       rdi, [r13+r14]
  je        collect_large_sieve_primes_inc
  ; Find the LSB.
  bsf       rcx, rdi
  ; Unset the LSB.
  lea       rax, [rdi-1]
  and       rax, rdi
  mov       [r13+r14], rax
  ; Move the prime into the segment array.
  xor       rax, rdi
  or        [r8+r14], rax
  ; Unset rdi for the next iteration.
  xor       rdi, rdi
  ; We found a prime! Figure out the value.
  mov       rax, r14                ; |
  shl       rax, 3                  ; | convert from byte count to bit count.
  add       rcx, rax                ; |
  lea       r12, [rcx+rcx+1]        ; | p = (c+x*8)*2+1
  ; This prime is inside the segment, add it to sieved_primes.
  mov       rax, r12                ; |
  mul       rax                     ; | f = p*p (next factor to look at)
  shr       rax, 1                  ; | a = f/2
  mov       [r11+r15], rax          ; |
  mov       [r11+r15+8], r12d       ; | sieve_primes[n/16] = (f/2, p)
  add       r15, 16                 ; n += 16
  jmp       collect_large_sieve_primes_loop

; Now that we've found the sieve primes, iterate over all segments find the
; rest.
all_segments:
  xor       rbx, rbx                ; segment_start_bits = 0
  vmovq     xmm0, rbx               ; output_buffer = 0
  vmovq     xmm1, rbx               ; prev_prime = 0
  ; We want to use print_segment for the first segment as well.
  ; However, it doesn't handle single digit output so we do those manually and
  ; cross them off the list here.
  lea       r13, [rel segment_array]
  and       [r13], byte 0xF0
  mov       [rsp+SIEVE_PRIME_BYTES_VAR], r15d
  jmp       print_segment

all_segments_loop:

; Find the primes in the next segment by sieving out all of the sieve primes.
handle_segment:
  mov       r15d, [rsp+SIEVE_PRIME_BYTES_VAR]

  ; Copy enough 8-byte elements to hold an offset wheel.
  mov       eax, [rsp+WHEEL_SIZE_BITS_VAR] ; |
  add       rax, SEGMENT_SIZE_BITS         ; |
  shr       rax, 3+3                       ; | (8 bits * 8 bytes)
  add       rax, 1                         ; | Padding for misalignment.
  memcpy_q  [rel template_segment_array], \
            [rel segment_array], \
            rax

  ; Increment the offset for alignment with the wheel.
  xor       eax, eax                         ; |
  mov       edx, [rsp+WHEEL_OFFSET_BITS_VAR] ; |
  mov       ecx, [rsp+WHEEL_DEC_BITS_VAR]    ; |
  sub       edx, ecx                         ; | offset -= dec
  cmovl     eax, [rsp+WHEEL_SIZE_BITS_VAR]   ; |
  add       edx, eax                         ; | if (offset < 0) offset += w
  mov       [rsp+WHEEL_OFFSET_BITS_VAR], edx ; |

  mov       r8, rbx                     ; |
  sub       r8, rdx                     ; | array_start_bits = segment_start_bits-array_offset_bits
  lea       r9, [rdx+ARRAY_SIZE_BITS]   ; | wheel_end_bits (where to stop clearing).
  lea       r13, [rel segment_array]

  xor       r14, r14                    ; x = 0 (index into sieve_primes)
  lea       r11, [rel sieve_primes]
align 16
handle_segment_loop:
  cmp       r14, r15
  jge       print_segment           ; if (x >= n) print_segment
  mov       rcx, [r11+r14]          ; |
  mov       r12d, [r11+r14+8]       ; | (f/2, p) = sieve_primes[x/16]
  add       r14, 16                 ; x += 16
  sub       rcx, r8                 ; c = f/2-array_start_bits
  ; Check if this multiple is too large for the segment.
  cmp       rcx, r9
  ; NOTE: We could do an extra check to see if we can finish this segment
  ; entirely, but currently that doesn't seem to help.
  jge       handle_segment_loop

  clear_prime_multiples all_segments, r9
  add       rcx, r8                 ; | Save the updated value of f back.
  mov       [r11+r14-16], rcx       ; | f/2 = c+array_start_bits
  jmp       handle_segment_loop

; Print the primes in the current segment.
print_segment:
  lea       rsi, [rel print_buffer]   ; buf = print_buffer
  lea       r10, [rel bcd_lookup]
  vmovq     rdi, xmm0                 ; bcd_buffer
  vmovq     r11, xmm1                 ; prev_prime
  mov       r8, [rsp+OUTPUT_LEN_VAR]
  mov       r9, [rsp+NEXT_POW_VAR]
  mov       r14, -8                   ; | x = -8 (byte index into initial_segment_array)
  ; Determine the wheel alignment
  mov       r15d, [rsp+WHEEL_OFFSET_BITS_VAR]
  lea       rax, [rel template_segment_array]
  ; Move r13 forward so that the first 8-byte section is valid.
  ; Adjust r15 to account for this.
  mov       rax, r15                  ; a = wheel_offset_bits
  and       rax, -64                  ; align a at an 8-byte boundary.
  shr       rax, 3                    ; |
  add       r13, rax                  ; | *sieve_array += a/8
  shl       rax, 3                    ; |
  sub       r15, rax                  ; | wheel_offset_bits -= a
  ; Zero out the invalid bits of r15 bits.
  mov       rcx, r15
  shr       qword [r13], cl
  shl       qword [r13], cl
  ; Calculate the global offset
  sub       r15, rbx                  ; | global_offset = wheel_offset_bits - segment_start_bits

  ; Put a sentinal at the end to avoid an extra loop check.
  mov       [r13+ARRAY_SIZE_BYTES+8], byte 0xFF

  ; Update segment_start_bits, this lets us know when to stop printing.
  add       rbx, SEGMENT_SIZE_BITS            ; |
  cmp       rbx, [rsp+SEARCH_LIMIT_BITS_VAR]  ; |
  cmovg     rbx, [rsp+SEARCH_LIMIT_BITS_VAR]  ; | segment_limit = min(
                                              ; |   segment_start_bits + segment_size_bits,
                                              ; |   search_limit_bits
                                              ; | )

  xor       rdx, rdx
align 16
print_segment_loop_inc:
  ; Find the next non-zero quad.
  add       r14, 8
print_segment_loop:
  ; NOTE: rdx must be cleared before calling print_segment_loop
  add       rdx, [r13+r14]
  jz        print_segment_loop_inc
  ; Find the LSB.
  bsf       rcx, rdx
  ; Unset the LSB.
  lea       rax, [rdx-1]
  and       rax, rdx
  mov       [r13+r14], rax
print_segment_found:
  ; We found a prime! Figure out the value.
  mov       rax, r14                         ; |
  shl       rax, 3                           ; | convert from byte count to bit count.
  add       rcx, rax                         ; |
  sub       rcx, r15                         ; | c = c+x*8-wheel_offset_bits+segment_start_bits
  cmp       rcx, rbx                         ; |
  jge       print_segment_write              ; | if (c > segment_limit) print_segment_write
  lea       r12, [rcx+rcx+1]                 ; | p = c*2+1
  ; Update output length variables.
  cmp       r12, r9                 ; | if (p >= next_pow) {
  jl        print_segment_itoa      ; |
  inc       r8                      ; |   output_len++
  imul      r9, 10                  ; |   next_pow *= 10
  ; Convert a number to string.     ; | }
  ; Here we refer to p as b, as we destroy while outputting.
print_segment_itoa:
  mov       rdx, r12                ; | delta = p
  sub       rdx, r11                ; | delta = p-prev
  and       rdx, 1023               ; | TODO: Remove (limit deltas)
  mov       r11, r12                ; | p = prev
  mov       eax, [r10+rdx*4]        ; |
  bcd_add   rdi, rax, rdx           ; | bcd_buffer += bcd[delta]
  mov       rax, rdi                ; |
  bswap     rax                     ; | a = bcd_buffer in output byte order.
  lea       rcx, [r8*8]             ; |
  rol       rax, cl                 ; | Shift number to the front
  mov       rcx,  0x3030303030303030; |
  or        rax, rcx                ; | Add 48 to convert to ascii
  mov       [rsi], rax              ; | Store all bytes but only update
  mov       [rsi+r8], byte `\n`     ; | buf enough to cover the number.
  lea       rsi, [rsi+r8+1]         ; |
  ; Clear rdx to prefer for the print_segment loop.
  xor       rdx, rdx
  jmp print_segment_loop

print_segment_write:
  ; Update local variables.
  mov       [rsp+OUTPUT_LEN_VAR], r8
  mov       [rsp+NEXT_POW_VAR], r9
  vmovq     xmm0, rdi
  vmovq     xmm1, r11
  ; If we have nothing to write, don't call _puts because it adds a newline.
  lea       rdi, [rel print_buffer]
  cmp       rsi, rdi
  je        print_segment_skip

  xmm_save
%ifndef QUIET
  ; Add a null byte to terminate the string.
  mov       [rsi-1], byte 0
  call _puts
%endif
  xmm_restore
print_segment_skip:

; Continue looping until we reach the search limit.
  cmp       rbx, [rsp+SEARCH_LIMIT_BITS_VAR]
  jl        all_segments_loop

exit:
  add       rsp, STACK_VAR_BYTES
  pop       rsp                     ; Fix up stack before returning
  xor       rax, rax                ; return 0
  ret

section   .data

; Small primes to directly print.
prelude:
  db `2\n3\n5\n7`, 0

; Format strings for debugging.
format_i64:
  db `> %ld\n`, 0
format_hex:
  db `> 0x%lx\n`, 0
format_sep:
  db `----\n`, 0

; Lookup table mapping n in [0, 100) to 2 character ascii strings.
align 16
digit_pair_lookup:
    db "0001020304050607080910111213141516171819"
    db "2021222324252627282930313233343536373839"
    db "4041424344454647484950515253545556575859"
    db "6061626364656667686970717273747576777879"
    db "8081828384858687888990919293949596979899"

section .bss

; Template array to hold the prime wheel.
; This is used to initialize the segment_array before processing each segment.
; Space is left at the end to be able to store an extra rotations of a wheel
; plus a byte for storing a sentinal value.
  alignb 64
template_segment_array:
  resb ARRAY_SIZE_BYTES*2+1

; The array used to sieving.
; Enough space for processing a single segment, with a buffer so that we can
; offset our use based on the position in the wheel.
  alignb 64
segment_array:
  resb ARRAY_SIZE_BYTES
; Array used for the initial segment.
; Also acts as a buffer for segment array, so it can be offset.
initial_segment_array:
  resb ARRAY_SIZE_BYTES+1 ; buffer

; Primes used for sieving.
  alignb 64
sieve_primes:
  ; Store pairs (f/2: u64, p: u32) where:
  ;  - f is the next candidate to be removed
  ;  - p is the prime
  ;  NOTE: It's difficult to find a way to represent f so that it will reliably
  ;        fit in 32 bits.
  ;        Storing it like this allows us to make the inner clearing loop smaller.
  resb MAX_PRIMES_PER_SEGMENT*16


  alignb 16
bcd_lookup:
  resd MAX_PRIME_GAP

  alignb 64
xmm_save_space:
  resb 1024

; Buffer space to write the output string.
  alignb 64
print_buffer:
  ; 20 bytes is enough to store 2**64
  resb MAX_PRIMES_PER_SEGMENT*20