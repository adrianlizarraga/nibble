
// r = a + b
/*
    r2 = r0 + r1

    mov r2, r0
    add r2, r1

    r2 = r0 - r1

    mov r2, r0
    sub r2, r1

    r2 = r0 * r1

    mov r2, r0
    imul r2, r1

    r2 = r0 / r1

    if size >= 2 bytes

    mov _ax, r0 // reserve rax and rdx (if size >= 2)
    cqo ; sign extend into _dx
    div r1
    mov r2, _ax
    
*/
