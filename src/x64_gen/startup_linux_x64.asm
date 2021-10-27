SECTION .text
global _start
_start:                          ; Program entry
    xor rbp, rbp                 ; Mark the end of linked stacked frames
    mov edi, dword [rsp]         ; Store argc into edi
    lea rsi, [rsp + 8]           ; Store argv (address of first char*) into rsi
    lea rdx, [rsp + 8*rdi + 16]  ; Store address of envp into rdx (8-bytes between argvs and envp)
    xor eax, eax                 ; Clear eax (just in case for variadic functions)
    call main                    ; Call program main()
    mov edi, eax                 ; Move main's return value into rdi for exit syscall
    mov rax, 60                  ; Move id of exit syscall into rax
    syscall                      ; Call exit syscall

global _nibble_#writeout
_nibble_#writeout:
    push rbp 
    mov rbp, rsp

    xchg rdi, rsi ; swap args -> bytes in rsi, count in rdi
    mov rax, 1    ; write syscall
    mov rdx, rdi  ; count in rdx
    mov rdi, 1    ; STDOUT_FILENO in rdi

    syscall

    mov rsp, rbp
    pop rbp
    ret

global _nibble_#readin
_nibble_#readin:
    push rbp 
    mov rbp, rsp

    xchg rdi, rsi ; swap args -> bytes in rsi, count in rdi
    mov rax, 0    ; read syscall
    mov rdx, rdi  ; count in rdx
    mov rdi, 0    ; STDIN_FILENO in rdi

    syscall

    mov rsp, rbp
    pop rbp
    ret
