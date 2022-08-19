; nasm -f elf64 tests/printf_call.s -o out_p.o
; ld out_p.o -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc -pie -o out_p
default rel
extern puts

SECTION .rodata
string: db "hello nibble\n", 0

SECTION .data
g_x: db 10
g_y: db 11

SECTION .text
global _start
_start:
  xor rbp, rbp

  mov eax, [rel g_x]
  mov eax, [rel g_y]
  xor eax, eax  ; No vector args to variadic function
  lea rdi, [rel string]
  call puts wrt ..plt
  ;call [rel puts wrt ..got]
  ;call puts ; Linker error

  xor eax, eax ; Return 0
  mov rax, 60
  syscall ; exit
