extern GetCommandLineA
extern LocalAlloc

; proc _nibble_get_win64_cmd_args(argc: ^int) => ^^char;
; Sets argc and returns argv
; argv should be freed with LocalFree()
_nibble_get_win64_cmd_args:
        push    r12
        xor     eax, eax
        push    rbp
        mov     rbp, rdi
        push    rbx
        call    GetCommandLineA
        cmp     BYTE PTR [rax], 34
        mov     r12, rax
        lea     rcx, [rax+1]
        jne     .l_winargs_84
.l_winargs_3:
        mov     al, BYTE PTR [rcx]
        test    al, al
        je      .l_winargs_4
        inc     rcx
        cmp     al, 34
        jne     .l_winargs_3
        jmp     .l_winargs_4
.l_winargs_84:
        mov     rcx, rax
.l_winargs_2:
        mov     al, BYTE PTR [rcx]
        test    al, -33
        je      .l_winargs_4
        cmp     al, 9
        je      .l_winargs_4
        inc     rcx
        jmp     .l_winargs_2
.l_winargs_9:
        inc     rcx
.l_winargs_4:
        mov     al, BYTE PTR [rcx]
        cmp     al, 32
        je      .l_winargs_9
        cmp     al, 9
        je      .l_winargs_9
        cmp     al, 1
        mov     r8d, 3
        sbb     esi, esi
        xor     eax, eax
        xor     edx, edx
        add     esi, 2
.l_winargs_11:
        mov     dil, BYTE PTR [rcx]
        test    dil, dil
        je      .l_winargs_85
        cmp     dil, 32
        sete    r9b
        cmp     dil, 9
        sete    r10b
        or      r9b, r10b
        je      .l_winargs_14
        test    dl, 1
        jne     .l_winargs_14
.l_winargs_12:
        mov     al, BYTE PTR [rcx]
        cmp     al, 32
        jne     .l_winargs_86
.l_winargs_15:
        inc     rcx
        jmp     .l_winargs_12
.l_winargs_86:
        cmp     al, 9
        je      .l_winargs_15
        test    al, al
        je      .l_winargs_49
        inc     esi
        jmp     .l_winargs_49
.l_winargs_14:
        inc     rcx
        cmp     dil, 92
        jne     .l_winargs_17
        inc     eax
        jmp     .l_winargs_11
.l_winargs_17:
        cmp     dil, 34
        jne     .l_winargs_50
        and     eax, 1
        cmp     eax, 1
        adc     edx, 0
.l_winargs_19:
        cmp     BYTE PTR [rcx], 34
        jne     .l_winargs_87
        inc     edx
        inc     rcx
        jmp     .l_winargs_19
.l_winargs_87:
        mov     eax, edx
        cdq
        idiv    r8d
        xor     eax, eax
        cmp     edx, 2
        jne     .l_winargs_11
        jmp     .l_winargs_81
.l_winargs_49:
        xor     eax, eax
.l_winargs_81:
        xor     edx, edx
        jmp     .l_winargs_11
.l_winargs_50:
        xor     eax, eax
        jmp     .l_winargs_11
.l_winargs_85:
        xor     edx, edx
.l_winargs_22:
        cmp     BYTE PTR [r12+rdx], 0
        je      .l_winargs_88
        inc     rdx
        jmp     .l_winargs_22
.l_winargs_88:
        lea     eax, [rsi+1]
        xor     edi, edi
        cdqe
        lea     rbx, [0+rax*8]
        lea     rsi, [rbx+1+rdx]
        call    LocalAlloc
        mov     r8, rax
        test    rax, rax
        je      .l_winargs_1
        lea     rax, [rax+rbx]
        xor     edx, edx
.l_winargs_25:
        mov     cl, BYTE PTR [r12+rdx]
        lea     rsi, [rax+rdx]
        test    cl, cl
        je      .l_winargs_89
        mov     BYTE PTR [rax+rdx], cl
        inc     rdx
        jmp     .l_winargs_25
.l_winargs_89:
        mov     BYTE PTR [rsi], 0
        mov     QWORD PTR [r8], rax
        cmp     BYTE PTR [rax], 34
        jne     .l_winargs_27
        inc     rax
.l_winargs_28:
        mov     cl, BYTE PTR [rax]
        lea     rdx, [rax-1]
        test    cl, cl
        je      .l_winargs_29
        inc     rax
        cmp     cl, 34
        je      .l_winargs_29
        mov     BYTE PTR [rax-2], cl
        jmp     .l_winargs_28
.l_winargs_90:
        cmp     cl, 9
        je      .l_winargs_29
.l_winargs_27:
        mov     cl, BYTE PTR [rax]
        mov     rdx, rax
        inc     rax
        test    cl, -33
        jne     .l_winargs_90
        test    cl, cl
        cmove   rax, rdx
.l_winargs_29:
        mov     BYTE PTR [rdx], 0
.l_winargs_32:
        mov     cl, BYTE PTR [rax]
        cmp     cl, 32
        jne     .l_winargs_91
.l_winargs_33:
        inc     rax
        jmp     .l_winargs_32
.l_winargs_91:
        cmp     cl, 9
        je      .l_winargs_33
        test    cl, cl
        jne     .l_winargs_34
        mov     QWORD PTR [r8+8], 0
        mov     DWORD PTR [rbp+0], 1
        jmp     .l_winargs_1
.l_winargs_34:
        inc     rdx
        xor     esi, esi
        xor     r9d, r9d
        mov     r11d, 2
        mov     QWORD PTR [r8+8], rdx
.l_winargs_35:
        mov     dil, BYTE PTR [rax]
        test    dil, dil
        je      .l_winargs_92
        cmp     dil, 32
        lea     r10, [rax+1]
        sete    cl
        cmp     dil, 9
        sete    bl
        or      cl, bl
        je      .l_winargs_36
        test    r9b, 1
        jne     .l_winargs_36
        mov     BYTE PTR [rdx], 0
        lea     rcx, [rdx+1]
        mov     r10, rax
.l_winargs_72:
        mov     al, BYTE PTR [r10+1]
        inc     r10
        cmp     al, 32
        je      .l_winargs_72
        cmp     al, 9
        je      .l_winargs_72
        test    al, al
        je      .l_winargs_53
        movsx   rax, r11d
        inc     r11d
        mov     QWORD PTR [r8+rax*8], rcx
        jmp     .l_winargs_53
.l_winargs_36:
        cmp     dil, 92
        jne     .l_winargs_39
        mov     BYTE PTR [rdx], 92
        lea     rcx, [rdx+1]
        inc     esi
        jmp     .l_winargs_38
.l_winargs_39:
        cmp     dil, 34
        jne     .l_winargs_40
        mov     eax, esi
        sar     eax
        and     sil, 1
        cdqe
        jne     .l_winargs_41
        sub     rdx, rax
        inc     r9d
        mov     rcx, rdx
        jmp     .l_winargs_43
.l_winargs_41:
        not     rax
        add     rax, rdx
        mov     BYTE PTR [rax], 34
        lea     rcx, [rax+1]
        jmp     .l_winargs_43
.l_winargs_45:
        inc     r9d
        cmp     r9d, 3
        jne     .l_winargs_44
        mov     BYTE PTR [rcx], 34
        xor     r9d, r9d
        inc     rcx
.l_winargs_44:
        inc     r10
.l_winargs_43:
        cmp     BYTE PTR [r10], 34
        je      .l_winargs_45
        xor     esi, esi
        cmp     r9d, 2
        jne     .l_winargs_38
        jmp     .l_winargs_82
.l_winargs_40:
        mov     BYTE PTR [rdx], dil
        lea     rcx, [rdx+1]
        xor     esi, esi
        jmp     .l_winargs_38
.l_winargs_53:
        xor     esi, esi
.l_winargs_82:
        xor     r9d, r9d
.l_winargs_38:
        mov     rdx, rcx
        mov     rax, r10
        jmp     .l_winargs_35
.l_winargs_92:
        movsx   rax, r11d
        mov     BYTE PTR [rdx], 0
        mov     QWORD PTR [r8+rax*8], 0
        mov     DWORD PTR [rbp+0], r11d
.l_winargs_1:
        pop     rbx
        mov     rax, r8
        pop     rbp
        pop     r12
        ret
