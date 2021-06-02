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
    R15
} Register;

Register arg_regs[] = {
    RDI, RSI, RDX, RCX, R8, R9
};

Register callee_saved_regs[] = {
    RBX, RBP, R12, R13, R14, R15
};

Register caller_saved_regs[] = {
    RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11
};

struct Generator {
    FILE* out_fd;
    char* out_buf;
    char* data_buf;

    uint64_t free_reg_mask;
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
    // Sum variable sizes for local variables declared in this scope.
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
        ftprint_err("Failed to write output file `%s`\n", output_file);
        return false;
    }

    emit("# Generated by Nibble compiler\n");

    gen_global_scope(scope);

    array_push(generator.out_buf, '\0');
    ftprint_file(generator.out_fd, false, "%s", generator.out_buf);

    array_free(generator.out_buf);
    array_free(generator.data_buf);

    return true;
}

