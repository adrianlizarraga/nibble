; nasm -f win64 .\win64_crt0.s
; link /entry:_start /nodefaultlib /subsystem:console .\win64_crt0.obj kernel32.lib user32.lib Shell32.lib

section .data
    message:     db 'Hello world!',10    ; 'Hello world!' plus a linefeed character
    messageLen equ $-message      ; Length of the 'Hello world!' string

    global _start
    extern  GetStdHandle
    extern  WriteConsoleW
    extern  ExitProcess
    extern  GetCommandLineW
    extern  CommandLineToArgvW
    extern  LocalFree

section .text

_start:
    ; At _start the stack is 8 bytes misaligned because there is a return
    ; address to the MSVCRT runtime library on the stack.
    push rbp
    mov rbp, rsp
    sub rsp, 32 + 32; alloc space for local variables and shadow space
                    ; int argc; (rbp - 4) and wchar** argv (rbp - 16) and u64 bytes_written (rbp - 24)

    call GetCommandLineW  ; rax contains pointer to entire command-line string.

    ; LPWSTR* CommandLineToArgvW(LPCWSTR lpCmdLine, int* pNumArgs);
    mov rcx, rax ; cmdline to rcx
    lea rdx, [rbp - 4] ; pointer to argc in rdx
    call CommandLineToArgvW
    mov qword [rbp - 16], rax ; store argv into stack

    ; For testing, let's get first string (pointer into rdx)
    mov rdx, qword [rax + 8]
  

    ; hStdOut = GetStdHandle(STD_OUTPUT_HANDLE)
    mov     ecx, -11
    call    GetStdHandle
    ;add rsp, 32 ; undo shadow space

    ; WriteFile(hstdOut, message, length(message), &bytes, 0);
    mov     rcx, rax
    ;mov     rdx, message
    mov     r8,  5 ; write 5 bytes
    lea     r9,  [rbp - 24]         ; Address for `bytes_written`
    sub rsp, 8 + 8; + 32 ;  8 for alignment, 8 for 5th stack arg, 32 shadow space (1st 4 args)
    mov     qword [rsp + 32], 0     ; First stack parameter of WriteConsoleW function
    call    WriteConsoleW
    ; add rsp, 8 + 8 + 32 ; restore stack pointer

    mov rcx, qword [rbp - 16] ; argv to rcx
    call LocalFree

    ; ExitProcess(0)
    ;    mov     rcx, 0
    ;    call    ExitProcess

    ; alternatively you can exit by setting RAX to 0
    ; and doing a ret

    mov rsp, rbp
    pop rbp
    xor eax, eax                  ; RAX = return value = 0
    ret