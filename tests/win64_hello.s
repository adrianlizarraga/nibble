section .data
    message:     db 'Hello world!',10    ; 'Hello world!' plus a linefeed character
    messageLen equ $-message      ; Length of the 'Hello world!' string

    global _start
    extern  GetStdHandle
    extern  WriteConsoleA
    extern  ExitProcess

section .text

_start:
    ; At _start the stack is 8 bytes misaligned because there is a return
    ; address to the MSVCRT runtime library on the stack.
    ; 8 bytes of temporary storage for `bytes`.
    ; allocate 32 bytes of stack for shadow space.
    ; 8 bytes for the 5th parameter of WriteConsole.
    ; An additional 8 bytes for padding to make RSP 16 byte aligned.
    sub     rsp, 8+8+8+32
    ; At this point RSP is aligned on a 16 byte boundary and all necessary
    ; space has been allocated.

    ; hStdOut = GetStdHandle(STD_OUTPUT_HANDLE)
    mov     ecx, -11
    call    GetStdHandle

    ; WriteFile(hstdOut, message, length(message), &bytes, 0);
    mov     rcx, rax
    mov     rdx, message
    mov     r8,  messageLen
    lea     r9,  [rsp-16]         ; Address for `bytes`
    ; RSP-17 through RSP-48 are the 32 bytes of shadow space
    mov     qword [rsp-56], 0     ; First stack parameter of WriteConsoleA function
    call    WriteConsoleA

    ; ExitProcess(0)
    ;    mov     rcx, 0
    ;    call    ExitProcess

    ; alternatively you can exit by setting RAX to 0
    ; and doing a ret

    add rsp, 8+8+32+8             ; Restore the stack pointer.
    xor eax, eax                  ; RAX = return value = 0
    ret