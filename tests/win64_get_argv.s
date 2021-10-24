extern GetCommandLineA
extern LocalFree
extern GetStdHandle
extern ReadFile
extern WriteFile
extern LocalAlloc

; proc _nibble_get_win64_cmd_args(argc: ^int) => ^^char;
; Sets argc and returns argv
; argv should be freed with LocalFree()
global _nibble_get_win64_cmd_args
_nibble_get_win64_cmd_args:
    push    r14
    push    rsi
    push    rdi
    push    rbx
    sub     rsp, 40
    mov     r14, rcx
    call    GetCommandLineA
    mov     rdi, rax
    mov     r8b, byte [rax]
    cmp     r8b, 34
    jne     .L_win64_args_1
    lea     rcx, [rdi + 1]
.L_win64_args_6:
    mov     al, byte [rcx]
    test    al, al
    je      .L_win64_args_8
    inc     rcx
    cmp     al, 34
    jne     .L_win64_args_6
    jmp     .L_win64_args_8
.L_win64_args_1:
    mov rax, 4294967809
    mov     edx, r8d
    mov     rcx, rdi
.L_win64_args_2:
    cmp     dl, 32
    ja      .L_win64_args_4
    movzx   edx, dl
    bt      rax, rdx
    jb      .L_win64_args_8
.L_win64_args_4:
    mov     dl, byte [rcx + 1]
    inc     rcx
    jmp     .L_win64_args_2
.L_win64_args_86:
    inc     rcx
.L_win64_args_8:
    mov     al, byte [rcx]
    cmp     al, 9
    je      .L_win64_args_86
    cmp     al, 32
    je      .L_win64_args_86
    test    al, al
    jne     .L_win64_args_12
    mov     edx, 1
    jmp     .L_win64_args_13
.L_win64_args_12:
    mov     edx, 2
.L_win64_args_13:
    xor     esi, esi
    xor     ebx, ebx
.L_win64_args_14:
    cmp     al, 32
    je      .L_win64_args_23
    test    al, al
    je      .L_win64_args_16
    cmp     al, 9
    jne     .L_win64_args_20
    test    esi, esi
    je      .L_win64_args_24
.L_win64_args_20:
    cmp     al, 34
    je      .L_win64_args_28
    cmp     al, 92
    jne     .L_win64_args_31
    inc     ebx
    inc     rcx
    jmp     .L_win64_args_33
.L_win64_args_23:
    test    esi, esi
    je      .L_win64_args_24
.L_win64_args_31:
    inc     rcx
    jmp     .L_win64_args_32
.L_win64_args_87:
    mov     al, byte [rcx + 1]
    inc     rcx
.L_win64_args_24:
    cmp     al, 9
    je      .L_win64_args_87
    cmp     al, 32
    je      .L_win64_args_87
    test    al, al
    je      .L_win64_args_32
    inc     edx
.L_win64_args_32:
    xor     ebx, ebx
.L_win64_args_33:
    mov     al, byte [rcx]
    jmp     .L_win64_args_14
.L_win64_args_28:
    not     ebx
    and     ebx, 1
    lea     eax, [rsi + rbx]
    dec     eax
.L_win64_args_29:
    inc     eax
    cmp     byte [rcx + 1], 34
    lea     rcx, [rcx + 1]
    je      .L_win64_args_29
    movsxd  rsi, eax
    imul    rax, rsi, 1431655766
    mov     rbx, rax
    shr     rbx, 63
    shr     rax, 32
    add     eax, ebx
    lea     eax, [rax + 2*rax]
    sub     esi, eax
    xor     ebx, ebx
    cmp     esi, 2
    cmove   esi, ebx
    jmp     .L_win64_args_33
.L_win64_args_16:
    test    r8b, r8b
    je      .L_win64_args_17
    mov     ecx, 1
.L_win64_args_35:
    lea     rax, [rcx + 1]
    cmp     byte [rdi + rcx], 0
    mov     rcx, rax
    jne     .L_win64_args_35
    jmp     .L_win64_args_36
.L_win64_args_17:
    mov     eax, 1
.L_win64_args_36:
    movsxd  rsi, edx
    lea     rdx, [rax + 8*rsi]
    add     rdx, 8
    xor     ecx, ecx
    call    LocalAlloc
    test    rax, rax
    je      .L_win64_args_37
    lea     rsi, [rax + 8*rsi]
    add     rsi, 8
    mov     cl, byte [rdi]
    test    cl, cl
    je      .L_win64_args_39
    inc     rdi
    mov     rdx, rsi
.L_win64_args_41:
    mov     byte [rdx], cl
    inc     rdx
    mov     cl, byte [rdi]
    inc     rdi
    test    cl, cl
    jne     .L_win64_args_41
    jmp     .L_win64_args_42
.L_win64_args_37:
    xor     eax, eax
    jmp     .L_win64_args_85
.L_win64_args_39:
    mov     rdx, rsi
.L_win64_args_42:
    mov     byte [rdx], 0
    mov     qword [rax], rsi
    mov     cl, byte [rsi]
    cmp     cl, 34
    jne     .L_win64_args_43
.L_win64_args_48:
    mov     cl, byte [rsi + 1]
    test    cl, cl
    je      .L_win64_args_52
    cmp     cl, 34
    je      .L_win64_args_50
    mov     byte [rsi], cl
    inc     rsi
    jmp     .L_win64_args_48
.L_win64_args_43:
    mov rdx, 4294967809
.L_win64_args_44:
    cmp     cl, 32
    ja      .L_win64_args_46
    movzx   ebx, cl
    bt      rdx, rbx
    jb      .L_win64_args_51
.L_win64_args_46:
    mov     cl, byte [rsi + 1]
    inc     rsi
    jmp     .L_win64_args_44
.L_win64_args_51:
    cmp     cl, 1
    mov     rcx, rsi
    sbb     rcx, -1
    jmp     .L_win64_args_53
.L_win64_args_52:
    lea     rcx, [rsi + 1]
    jmp     .L_win64_args_53
.L_win64_args_50:
    lea     rcx, [rsi + 2]
.L_win64_args_53:
    mov     byte [rsi], 0
    inc     rsi
.L_win64_args_54:
    mov     dl, byte [rcx]
    cmp     dl, 9
    je      .L_win64_args_88
    cmp     dl, 32
    jne     .L_win64_args_56
.L_win64_args_88:
    inc     rcx
    jmp     .L_win64_args_54
.L_win64_args_56:
    test    dl, dl
    jne     .L_win64_args_58
    mov     rcx, rax
    add     rcx, 8
    mov     r9d, 1
    jmp     .L_win64_args_84
.L_win64_args_58:
    mov     qword [rax + 8], rsi
    xor     r8d, r8d
    mov     r9d, 2
    xor     edi, edi
.L_win64_args_82:
    xor     ebx, ebx
.L_win64_args_60:
    mov     dl, byte [rcx]
    cmp     dl, 32
    je      .L_win64_args_68
    test    dl, dl
    je      .L_win64_args_83
    cmp     dl, 9
    jne     .L_win64_args_64
    test    edi, edi
    je      .L_win64_args_69
.L_win64_args_64:
    cmp     dl, 92
    jne     .L_win64_args_65
    inc     rcx
    mov     byte [rsi], 92
    inc     rsi
    inc     ebx
    jmp     .L_win64_args_60
.L_win64_args_68:
    test    edi, edi
    jne     .L_win64_args_81
.L_win64_args_69:
    mov     byte [rsi], 0
    inc     rsi
.L_win64_args_70:
    inc     rcx
    mov     dl, byte [rcx]
    cmp     dl, 9
    je      .L_win64_args_70
    cmp     dl, 32
    je      .L_win64_args_70
    test    dl, dl
    je      .L_win64_args_82
    mov     edx, r9d
    inc     r9d
    mov     qword [rax + 8*rdx], rsi
    jmp     .L_win64_args_82
.L_win64_args_65:
    cmp     dl, 34
    je      .L_win64_args_66
.L_win64_args_81:
    inc     rcx
    mov     byte [rsi], dl
    inc     rsi
    jmp     .L_win64_args_82
.L_win64_args_66:
    mov     edx, ebx
    shr     edx, 31
    add     edx, ebx
    sar  edx, 1
    neg     edx
    movsxd  rdx, edx
    test    bl, 1
    jne     .L_win64_args_74
    add     rsi, rdx
    inc     edi
    jmp     .L_win64_args_75
.L_win64_args_74:
    mov     byte [rdx + rsi - 1], 34
    add     rsi, rdx
.L_win64_args_75:
    lea     rbx, [rcx + 1]
    cmp     byte [rcx + 1], 34
    jne     .L_win64_args_76
.L_win64_args_77:
    inc     edi
    cmp     edi, 3
    jne     .L_win64_args_79
    mov     byte [rsi], 34
    inc     rsi
    xor     edi, edi
.L_win64_args_79:
    lea     rcx, [rbx + 1]
    cmp     byte [rbx + 1], 34
    mov     rbx, rcx
    je      .L_win64_args_77
    jmp     .L_win64_args_80
.L_win64_args_76:
    mov     rcx, rbx
.L_win64_args_80:
    cmp     edi, 2
    cmove   edi, r8d
    jmp     .L_win64_args_82
.L_win64_args_83:
    mov     byte [rsi], 0
    mov     ecx, r9d
    lea     rcx, [rax + 8*rcx]
.L_win64_args_84:
    mov     qword [rcx], 0
    mov     dword [r14], r9d
.L_win64_args_85:
    add     rsp, 40
    pop     rbx
    pop     rdi
    pop     rsi
    pop     r14
    ret

global _start
_start:  ; Program entry
    push rbp
    mov rbp, rsp
    sub rsp, 16 + 32             ; Alloc space for locals and shadow space (for all called funcs)
                                 ; argc: int (rbp - 4), retval : int (rbp - 8), argv: ^^s16 (rbp - 16)

    lea rcx, [rbp - 4]              ; rcx = ^argc
    call _nibble_get_win64_cmd_args ; rax = copy of argv
    mov qword [rbp - 16], rax       ; store argv to free later

    mov rcx, qword [rbp - 4]     ; rcx = argc
    mov rdx, rax                 ; rdx = argv
    xor eax, eax                 ; Clear eax (just in case for variadic functions)
    call main                    ; Call program main()
    mov dword [rbp - 8], eax     ; store retval

    mov rcx, qword [rbp - 16]    ; rcx = argv
    call LocalFree               ; free argv
 
    mov eax, dword [rbp - 8]     ; Return retval from main()

    mov rsp, rbp
    pop rbp
    ret