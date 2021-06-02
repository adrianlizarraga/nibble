#include "gen_assembly.h"

typedef struct Generator Generator;
typedef struct Operand Operand;

typedef enum Register {
    RAX = 0,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    REG_INVALID,
    REG_COUNT = REG_INVALID
} Register;

typedef enum RegisterFlag {
    ARG_REG = 0x1,
    RET_REG = 0x2,
    CALLEE_SAVED = 0x4,
} RegisterFlag;

static Register arg_regs[] = {
    RDI, RSI, RDX, RCX, R8, R9
};

static Register ret_regs[] = {
    RAX, RDX
};

static Register scratch_regs[] = {
    RDI, RSI, RDX, RCX, R8, R9, R10, R11, //RAX, RBX, R12, R13, R14, R15
};

static uint32_t reg_flags[] = {
    [RAX] = RET_REG,
    [RCX] = ARG_REG,
    [RDX] = ARG_REG | RET_REG,
    [RBX] = CALLEE_SAVED,
    [RSP] = CALLEE_SAVED,
    [RBP] = CALLEE_SAVED,
    [RSI] = ARG_REG,
    [RDI] = ARG_REG,
    [R8]  = ARG_REG,
    [R9]  = ARG_REG,
    [R12] = CALLEE_SAVED,
    [R13] = CALLEE_SAVED,
    [R14] = CALLEE_SAVED,
    [R15] = CALLEE_SAVED
};

struct Generator {
    FILE* out_fd;
    char* out_buf;
    char* data_buf;

    uint32_t free_reg_mask;
};

typedef enum OperandKind {
    OPERAND_NONE,
    OPERAND_FRAME_OFFSET,
    OPERAND_REGISTER,
    OPERAND_IMMEDIATE,
    OPERAND_GLOBAL_VAR,
} OperandKind;

struct Operand {
    OperandKind kind;
    Type* type;
    
    union {
        size_t offset;
        Register reg;
        Scalar imm;
        const char* var;
    };
};

static Generator generator;

#define emit(f, ...) ftprint_char_array(&generator.out_buf, false, (f), ## __VA_ARGS__)
#define emit_data(f, ...) ftprint_char_array(&generator.data_buf, false, (f), ## __VA_ARGS__)

static void gen_stmt(Stmt* stmt);

//  ntz() from Hacker's Delight 2nd edition, pg 108
//  Calculates the number of trailing zeros.
static int ntz(uint32_t x)
{
    if (x == 0)
        return 32;

    int n = 1;

    // Binary search:
    // Check for all zeros in right half of x. If all zeros, increment count and shift right.
    if ((x & 0x0000FFFF) == 0)
    {
        n += 16;
        x = x >> 16;
    }

    if ((x & 0x000000FF) == 0)
    {
        n += 8;
        x = x >> 8;
    }

    if ((x & 0x0000000F) == 0)
    {
        n += 4;
        x = x >> 4;
    }

    if ((x & 0x00000003) == 0)
    {
        n += 2;
        x = x >> 2;
    }

    return n - (x & 1);
}

static void alloc_reg(Register reg)
{
    generator.free_reg_mask &= ~(1 << reg);
}

static void free_reg(Register reg)
{
    generator.free_reg_mask |= 1 << reg;
}

static void init_regs()
{
    size_t num_scratch_regs = sizeof(scratch_regs) / sizeof(Register);

    for (size_t i = 0; i < num_scratch_regs; i += 1)
        free_reg(scratch_regs[i]);
}

static Register next_reg()
{
    Register reg = REG_INVALID;
    int bit_index = ntz(generator.free_reg_mask); 

    if (bit_index < REG_COUNT)
    {
        alloc_reg(bit_index);

        reg = (Register) bit_index;
    }
    else
    {
        // TODO: Spill registers
        ftprint_err("Out of free registers. TODO: spill registers\n");
        assert(0);
    }

    return reg;
}

static void emit_spill_reg(Register reg, size_t offset)
{
    
}

static void emit_data_seg()
{
    emit("# Data segment\n");
    emit(".data\n");

    array_push(generator.data_buf, '\0');

    emit("%s\n\n", generator.data_buf);
}

static void emit_data_value(Type* type, Scalar scalar)
{
    if (type == type_int)
    {
        emit_data(".long %d\n", scalar.as_int.i);
    }
    else
    {
        ftprint_err("Cannot gen GAS data regions for non-int types");
        assert(0);
    }
}

static void free_operand(Operand* operand)
{
    (void)operand;
}

static void gen_expr(Expr* expr, Operand* dest)
{
    (void)expr;
    (void)dest;
}

static void gen_stmt_return(Stmt* stmt)
{
    StmtReturn* sreturn = (StmtReturn*)stmt;

    Operand operand = {0};
    gen_expr(sreturn->expr, &operand);

    free_operand(&operand);
}

static void gen_stmt_block(Stmt* stmt)
{
    StmtBlock* sblock = (StmtBlock*)stmt;

    List* head = &sblock->stmts;
    List* it   = head->next;

    while (it != head)
    {
        Stmt* s = list_entry(it, Stmt, lnode);

        gen_stmt(s);

        it = it->next;
    }
}

static void gen_stmt(Stmt* stmt)
{
    switch (stmt->kind)
    {
        case CST_StmtBlock:
            gen_stmt_block(stmt);
            break;
        case CST_StmtReturn:
            gen_stmt_return(stmt);
            break;
        default:
            break;
    }
}

static size_t compute_scope_var_offsets(Scope* scope, size_t offset)
{
    size_t stack_size = offset;

    //
    // Sum sizes of local variables declared in this scope.
    //
    {
        List* head = &scope->sym_list;
        List* it   = head->next;

        while (it != head)
        {
            Symbol* sym = list_entry(it, Symbol, lnode);

            if (sym->kind == SYMBOL_VAR)
            {
                stack_size += sym->type->size;
                stack_size = ALIGN_UP(stack_size, sym->type->align);
                sym->offset = stack_size;
            }

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //
    {
        List* head = &scope->children;
        List* it   = head->next;
        size_t child_offset = stack_size;

        while (it != head)
        {
            Scope* child_scope = list_entry(it, Scope, lnode);
            size_t child_size  = compute_scope_var_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, 16);
}

static size_t compute_proc_var_offsets(DeclProc* dproc)
{

}

static void gen_proc(Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    size_t stack_size = compute_scope_var_offsets(dproc->scope, 0);

    emit("\n");
    emit(".text\n");
    emit(".globl %s\n", sym->name);
    emit("%s:\n", sym->name);

    emit("    push %%rbp\n");
    emit("    mov %%rsp, %%rbp\n");

    if (stack_size)
        emit("    sub $%d, %%rsp\n", stack_size);

    gen_stmt(dproc->body);

    emit("\n    end.%s:\n", sym->name);
    emit("    leave\n");
    emit("    ret\n");
}

static void gen_global_scope(Scope* scope)
{
    //
    // Generate global variables
    //
    List* head = &scope->sym_list;
    List* it   = head->next;

    while (it != head)
    {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (sym->kind == SYMBOL_VAR)
        {
            DeclVar* dvar = (DeclVar*)sym->decl;

            emit_data(".align %d\n", sym->type->align);
            emit_data("%s: ", sym->name);
            emit_data_value(sym->type, dvar->init->const_val);
        }

        it = it->next;
    }

    //
    // Generate procedures
    //
    it = head->next;

    while (it != head)
    {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (sym->kind == SYMBOL_PROC)
            gen_proc(sym);

        it = it->next;
    }

    emit_data_seg();
}

bool gen_gasm(Scope* scope, const char* output_file)
{
    generator.out_buf = array_create(NULL, char, 512);
    generator.data_buf = array_create(NULL, char, 512);
    generator.out_fd = fopen(output_file, "w");    

    if (!generator.out_fd)
    {
        ftprint_err("Failed to open output file `%s`\n", output_file);
        return false;
    }

    init_regs();

    emit("# Generated by Nibble compiler\n");

    gen_global_scope(scope);

    array_push(generator.out_buf, '\0');
    ftprint_file(generator.out_fd, false, "%s", generator.out_buf);

    array_free(generator.out_buf);
    array_free(generator.data_buf);

    return true;
}

