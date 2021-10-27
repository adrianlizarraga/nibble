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
global _nibble_#writeout
_nibble_#writeout:
push rbp 
mov rbp, rsp
xchg rdi, rsi
mov rax, 1
mov rdx, rdi
mov rdi, 1
syscall
mov rsp, rbp
pop rbp
ret
global _nibble_#readin
_nibble_#readin:
push rbp 
mov rbp, rsp
xchg rdi, rsi
mov rax, 0
mov rdx, rdi
mov rdi, 0
syscall
mov rsp, rbp
pop rbp
ret
