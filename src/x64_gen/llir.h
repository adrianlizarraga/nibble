#ifndef NIBBLE_X64_LIR_H
#define NIBBLE_X64_LIR_H

typedef enum X64_InstrKind {
    X64_INSTR_NONE = 0,

    // Addition
    X64_INSTR_ADD_R_R,
    X64_INSTR_ADD_R_M,
    X64_INSTR_ADD_R_I,

    // Subtraction
    X64_INSTR_SUB_R_R,
    X64_INSTR_SUB_R_M,
    X64_INSTR_SUB_R_I,

    // Multiplication
    X64_INSTR_IMUL_R_R,
    X64_INSTR_IMUL_R_M,
    X64_INSTR_IMUL_R_I,

    // Unsigned division
    X64_INSTR_DIV_R_R,
    X64_INSTR_DIV_R_M,
    X64_INSTR_DIV_R_I,

    // Signed division
    X64_INSTR_IDIV_R_R,
    X64_INSTR_IDIV_R_M,
    X64_INSTR_IDIV_R_I,

    // TODO: Unsigned/Signed MOD

    // Arithmetic shift right
    X64_INSTR_SAR_R_R,
    X64_INSTR_SAR_R_M,
    X64_INSTR_SAR_R_I,

    // Shift left
    X64_INSTR_SHL_R_R,
    X64_INSTR_SHL_R_M,
    X64_INSTR_SHL_R_I,

    // Bitwise AND
    X64_INSTR_AND_R_R,
    X64_INSTR_AND_R_M,
    X64_INSTR_AND_R_I,

    // Bitwise OR
    X64_INSTR_OR_R_R,
    X64_INSTR_OR_R_M,
    X64_INSTR_OR_R_I,

    // Bitwise XOR
    X64_INSTR_XOR_R_R,
    X64_INSTR_XOR_R_M,
    X64_INSTR_XOR_R_I,

    // Bitwise NOT
    X64_INSTR_NOT,

    // Two's complement negation.
    X64_INSTR_NEG,

    // Zero-extend
    X64_INSTR_MOVZX_R_R,
    X64_INSTR_MOVZX_R_M,

    // Sign-extend
    X64_INSTR_MOVSX_R_R,
    X64_INSTR_MOVSX_R_M,

    // Load an address computation into a register.
    X64_INSTR_LEA,

    X64_INSTR_MOV_R_I, // Load imm
    X64_INSTR_MOV_R_R, // Register copy
    X64_INSTR_MOV_R_M, // Load memory

    // Store into memory.
    X64_INSTR_MOV_M_R,
    X64_INSTR_MOV_M_I,

    // Compare two values and set condition flags
    X64_INSTR_CMP_R_R,
    X64_INSTR_CMP_R_M,
    X64_INSTR_CMP_R_I,
    X64_INSTR_CMP_M_R,
    X64_INSTR_CMP_M_I,

    // Jump to instruction index
    X64_INSTR_JMP,

    // Jump to instruction index based on condition
    X64_INSTR_JMPCC,

    // Set a byte (0 or 1) based on condition
    X64_INSTR_SETCC,

    // Return value in specifed register
    X64_INSTR_RET,

    // Call a procedure directly
    X64_INSTR_CALL,

    // Call a procedure indirectly (register has procedure address)
    X64_INSTR_CALL_R,

    // Copy memory (compile-time known size)
    X64_INSTR_MEMCPY,
} X64_InstrKind;

typedef enum X64_MemAddrKind {
    X64_ADDR_GLOBAL,
    X64_ADDR_LOCAL,
    X64_ADDR_STR_LIT,
} X64_MemAddrKind;

typedef struct X64_MemAddr {
    X64_MemAddrKind kind;

    union {
        Symbol* global;
        struct {
            u32 base_reg;
            u32 index_reg;
            s32 disp;
            u8 scale;
        } local;
        StrLit* str_lit;
    };
} X64_MemAddr;

typedef struct X64_InstrBinary_R_R {
    Type* type;
    u32 dst;
    u32 src;
} X64_InstrBinary_R_R;

typedef struct X64_InstrBinary_R_M {
    Type* type;
    u32 dst;
    X64_MemAddr src;
} X64_InstrBinary_R_M;

typedef struct X64_InstrBinary_R_I {
    Type* type;
    u32 dst;
    Scalar src;
} X64_InstrBinary_R_I;

typedef struct X64_InstrShift_R_R {
    Type* dst_type;
    Type* src_type;
    u32 dst;
    u32 src;
} X64_InstrShift_R_R;

typedef struct X64_InstrShift_R_M {
    Type* dst_type;
    Type* src_type;
    u32 dst;
    X64_MemAddr src;
} X64_InstrShift_R_M;

typedef struct X64_InstrShift_R_I {
    Type* dst_type;
    Type* src_type;
    u32 dst;
    Scalar src;
} X64_InstrShift_R_I;

typedef struct X64_InstrUnary {
    Type* type;
    u32 dst;
} X64_InstrUnary;

typedef struct X64_InstrLImm {
    Type* type;
    u32 dst;
    Scalar src;
} X64_InstrLImm;

typedef struct X64_InstrLoad {
    Type* type;
    u32 dst;
    X64_MemAddr src;
} X64_InstrLoad;

typedef struct X64_InstrStore_R {
    Type* type;
    X64_MemAddr dst;
    u32 src;
} X64_InstrStore_R;

typedef struct X64_InstrStore_I {
    Type* type;
    X64_MemAddr dst;
    Scalar src;
} X64_InstrStore_I;

typedef struct X64_InstrLEA {
    u32 dst;
    Type* type; // Type of the thing we would be loading. Not necessary??
    X64_MemAddr mem;
} X64_InstrLEA;

typedef struct X64_InstrConvert_R_R {
    Type* dst_type;
    Type* src_type;
    u32 dst;
    u32 src;
} X64_InstrConvert_R_R;

typedef struct X64_InstrConvert_R_M {
    Type* dst_type;
    Type* src_type;
    u32 dst;
    X64_MemAddr src;
} X64_InstrConvert_R_M;

typedef struct X64_InstrCmp_R_R {
    Type* type;
    u32 op1;
    u32 op2;
} X64_InstrCmp_R_R;

typedef struct X64_InstrCmp_R_M {
    Type* type;
    u32 op1;
    X64_MemAddr op2;
} X64_InstrCmp_R_M;

typedef struct X64_InstrCmp_R_I {
    Type* type;
    u32 op1;
    Scalar op2;
} X64_InstrCmp_R_I;

typedef struct X64_InstrCmp_M_R {
    Type* type;
    X64_MemAddr op1;
    u32 op2;
} X64_InstrCmp_M_R;

typedef struct X64_InstrCmp_M_I {
    Type* type;
    X64_MemAddr op1;
    Scalar op2;
} X64_InstrCmp_M_I;

typedef struct X64_InstrJmpCC {
    u32* jmp_target;
    ConditionKind cond;
} X64_InstrJmpCC;

typedef struct X64_InstrJmp {
    u32* jmp_target;
} X64_InstrJmp;

typedef struct X64_InstrSetCC {
    ConditionKind cond;
    u32 dst;
} X64_InstrSetCC;

typedef struct X64_InstrCallArg {
    Type* type;
    bool in_reg;
    
    union {
        u32 reg;
        u32 offset;
    };
} X64_InstrCallArg;

typedef struct X64_InstrCall {
    Symbol* sym;
    u32 num_args;
    X64_InstrCallArg* args;
} X64_InstrCall;

typedef struct X64_InstrCall_R {
    Type* proc_type;
    u32 proc_loc;
    u32 num_args;
    X64_InstrCallArg* args;
} X64_InstrCall_R;

typedef struct X64_Instr {
    X64_InstrKind kind;
    bool is_jmp_target;

    union {
        // Addition
        X64_InstrBinary_R_R add_r_r;
        X64_InstrBinary_R_M add_r_m;
        X64_InstrBinary_R_I add_r_i;

        // Subtraction
        X64_InstrBinary_R_R sub_r_r;
        X64_InstrBinary_R_M sub_r_m;
        X64_InstrBinary_R_I sub_r_i;

        // Multiplication
        X64_InstrBinary_R_R mul_r_r;
        X64_InstrBinary_R_M mul_r_m;
        X64_InstrBinary_R_I mul_r_i;

        // Division
        X64_InstrBinary_R_R div_r_r;
        X64_InstrBinary_R_M div_r_m;
        X64_InstrBinary_R_I div_r_i;

        // Arithmetic shift right
        X64_InstrShift_R_R sar_r_r;
        X64_InstrShift_R_M sar_r_m;
        X64_InstrShift_R_I sar_r_i;

        // Shift left
        X64_InstrShift_R_R shl_r_r;
        X64_InstrShift_R_M shl_r_m;
        X64_InstrShift_R_I shl_r_i;

        // Bitwise AND
        X64_InstrBinary_R_R and_r_r;
        X64_InstrBinary_R_M and_r_m;
        X64_InstrBinary_R_I and_r_i;

        // Bitwise OR
        X64_InstrBinary_R_R or_r_r;
        X64_InstrBinary_R_M or_r_m;
        X64_InstrBinary_R_I or_r_i;

        // Bitwise XOR
        X64_InstrBinary_R_R xor_r_r;
        X64_InstrBinary_R_M xor_r_m;
        X64_InstrBinary_R_I xor_r_i;

        // Two's complement negation
        X64_InstrUnary neg;

        // Bitwise NOT
        X64_InstrUnary not ;

        // Load immediate into register
        X64_InstrLImm limm;

        // Load an address computation into register
        X64_InstrLAddr laddr;

        // Truncate integer
        X64_InstrConvert_R_R trunc_r_r;
        X64_InstrConvert_R_M trunc_r_m;

        // Zero-extend integer
        X64_InstrConvert_R_R zext_r_r;
        X64_InstrConvert_R_M zext_r_m;

        // Sign-extend integer
        X64_InstrConvert_R_R sext_r_r;
        X64_InstrConvert_R_M sext_r_m;

        // Load contents of memory into register
        X64_InstrLoad load;

        // Store register contents into memory
        X64_InstrStore_R store_r;

        // Store immediate into memory
        X64_InstrStore_I store_i;

        // Compare two values and set condition flags
        X64_InstrCmp_R_R cmp_r_r;
        X64_InstrCmp_R_M cmp_r_m;
        X64_InstrCmp_R_I cmp_r_i;
        X64_InstrCmp_M_R cmp_m_r;
        X64_InstrCmp_M_I cmp_m_i;

        // Jump to instruction index
        X64_InstrJmp jmp;

        // Jump to instruction index based on condition
        X64_InstrJmpCC jmpcc;

        // Set a byte (1 or 0) based on condition
        X64_InstrSetCC setcc;

        // Return value in specified register
        X64_InstrRet ret;

        // Call a procedure directly
        X64_InstrCall call;

        // Call a procedure indirectly (register contains procedure address)
        X64_InstrCall_R call_r;

        // Copy memory
        X64_InstrMemcpy memcpy;
    };
} X64_Instr;
#endif
