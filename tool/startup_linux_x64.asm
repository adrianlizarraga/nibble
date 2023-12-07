SECTION .text
global _start
_start:
  xor rbp, rbp
  mov edi, dword [rsp]
  lea rsi, [rsp + 8]
  lea rdx, [rsp + 8*rdi + 16]
  xor eax, eax
  call main
  mov edi, eax
  mov rax, 60
  syscall
