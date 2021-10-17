#include "cstring.h"
#include "x64_gen/regs.h"

// Linux System V ABI
static X64_Reg x64_linux_leaf_scratch_regs[] = {
    X64_R10, X64_R11, X64_RAX, X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, // NOTE: Caller saved
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, // NOTE: Callee saved
};

static X64_Reg x64_linux_nonleaf_scratch_regs[] = {
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, // NOTE: Callee saved
    X64_R10, X64_R11, X64_RAX, X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, // NOTE: Caller saved
};

static X64_Reg x64_linux_arg_regs[] = {X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9};

// Bit is 1 for caller saved registers: RAX, RCX, RDX, _, _, _, RSI, RDI, R8, R9, R10, R11, _, _, _, _
static const u32 x64_linux_caller_saved_reg_mask = 0x0FC7;

// Bit is 1 for arg registers: _, RCX, RDX, _, _, _, RSI, RDI, R8, R9, _, _, _, _, _, _
static const u32 x64_linux_arg_reg_mask = 0x03C6;

// Windows ABI
static X64_Reg x64_windows_leaf_scratch_regs[] = {
    X64_R10, X64_R11, X64_RAX, X64_RCX, X64_RDX, X64_R8,  X64_R9, // NOTE: Caller saved
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, X64_RSI, X64_RDI, // NOTE: Callee saved
};

static X64_Reg x64_windows_nonleaf_scratch_regs[] = {
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, X64_RSI, X64_RDI, // NOTE: Callee saved
    X64_R10, X64_R11, X64_RAX, X64_RCX, X64_RDX, X64_R8,  X64_R9, // NOTE: Caller saved
};

static X64_Reg x64_windows_arg_regs[] = {X64_RCX, X64_RDX, X64_R8, X64_R9};

// RAX, RCX, RDX, _, _, _, _, _, R8, R9, R10, R11, _, _, _, _
static const u32 x64_windows_caller_saved_reg_mask = 0x0F07;

// _, RCX, RDX, _, _, _, _, _, R8, R9, _, _, _, _, _, _
static const u32 x64_windows_arg_reg_mask = 0x0306;

static const char* x64_linux_startup_code =
    "SECTION .text\n"
    "global _start\n"
    "_start:  ; Program entry\n"
    "    xor rbp, rbp                 ; Mark the end of linked stacked frames\n"
    "    mov edi, dword [rsp]         ; Store argc into edi\n"
    "    lea rsi, [rsp + 8]           ; Store argv (address of first char*) into rsi\n"
    "    lea rdx, [rsp + 8*rdi + 16]  ; Store address of envp into rdx (8-bytes between argvs and envp)\n"
    "    xor eax, eax                 ; Clear eax (just in case for variadic functions)\n"
    "    call main                    ; Call program main()\n"
    "    mov edi, eax                 ; Move main's return value into rdi for exit syscall\n"
    "    mov rax, 60                  ; Move id of exit syscall into rax\n"
    "    syscall                      ; Call exit syscall\n"
    "\n"
    "global _nibble_stdout\n"
    "_nibble_stdout:\n"
    "    push rbp \n"
    "    mov rbp, rsp\n"
    "\n"
    "    xchg rdi, rsi ; swap args -> bytes in rsi, count in rdi\n"
    "    mov rax, 1    ; write syscall\n"
    "    mov rdx, rdi  ; count in rdx\n"
    "    mov rdi, 1    ; STDOUT_FILENO in rdi\n"
    "\n"
    "    syscall\n"
    "\n"
    "    mov rsp, rbp\n"
    "    pop rbp\n"
    "    ret\n";

// TODO: https://stackoverflow.com/questions/59474618/hello-world-in-nasm-with-link-exe-and-winapi
static const char* x64_windows_startup_code =
    "SECTION .text\n"
    "global _start\n"
    "_start:  ; Program entry\n"
    "    xor rbp, rbp                 ; Mark the end of linked stacked frames\n"
    "    ; TODO - GET argc and argv\n"
    "    xor eax, eax                 ; Clear eax (just in case for variadic functions)\n"
    "    sub rsp, 32                  ; Shadow space\n"
    "    call main                    ; Call program main()\n"
    "    ret\n"
    "    ; TODO - EXIT\n";

X64_Target x64_target;

bool init_x64_target(OS target_os)
{
    x64_target.os = target_os;

    switch (target_os) {
    case OS_LINUX:
        x64_target.num_arg_regs = ARRAY_LEN(x64_linux_arg_regs);
        x64_target.arg_regs = x64_linux_arg_regs;

        x64_target.num_leaf_scratch_regs = ARRAY_LEN(x64_linux_leaf_scratch_regs);
        x64_target.leaf_scratch_regs = x64_linux_leaf_scratch_regs;

        x64_target.num_nonleaf_scratch_regs = ARRAY_LEN(x64_linux_nonleaf_scratch_regs);
        x64_target.nonleaf_scratch_regs = x64_linux_nonleaf_scratch_regs;

        x64_target.caller_saved_reg_mask = x64_linux_caller_saved_reg_mask;
        x64_target.arg_reg_mask = x64_linux_arg_reg_mask;

        x64_target.startup_code = x64_linux_startup_code;
        return true;
    case OS_WIN32:
        x64_target.num_arg_regs = ARRAY_LEN(x64_windows_arg_regs);
        x64_target.arg_regs = x64_windows_arg_regs;

        x64_target.num_leaf_scratch_regs = ARRAY_LEN(x64_windows_leaf_scratch_regs);
        x64_target.leaf_scratch_regs = x64_windows_leaf_scratch_regs;

        x64_target.num_nonleaf_scratch_regs = ARRAY_LEN(x64_windows_nonleaf_scratch_regs);
        x64_target.nonleaf_scratch_regs = x64_windows_nonleaf_scratch_regs;

        x64_target.caller_saved_reg_mask = x64_windows_caller_saved_reg_mask;
        x64_target.arg_reg_mask = x64_windows_arg_reg_mask;

        x64_target.startup_code = x64_windows_startup_code;
        return true;
    default:
        return false;
    }
}

bool X64_is_caller_saved_reg(X64_Reg reg)
{
    return u32_is_bit_set(x64_target.caller_saved_reg_mask, reg);
}

bool X64_is_callee_saved_reg(X64_Reg reg)
{
    return !u32_is_bit_set(x64_target.caller_saved_reg_mask, reg);
}

bool X64_is_arg_reg(X64_Reg reg)
{
    return u32_is_bit_set(x64_target.arg_reg_mask, reg);
}
