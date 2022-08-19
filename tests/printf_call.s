extern printf

SECTION .rodata
string: db "hello nibble\n", 0

SECTION .text
global main
main:
    push rbp
    mov rbp, rsp

    xor eax, eax  ; No vector args to variadic function
    lea rdi, [rel string]
    call printf

    xor eax, eax ; Return 0
    pop rbp
    ret
