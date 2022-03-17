#.section .rodata
.data
string: .asciz "hello nibble\n"

.text
.globl main
main:
    push %rbp
    movq %rsp, %rbp

    xor %eax, %eax  # No vector args to variadic function
    lea string(%rip), %rdi
    call printf

    xor %eax, %eax # Return 0
    pop %rbp
    ret
