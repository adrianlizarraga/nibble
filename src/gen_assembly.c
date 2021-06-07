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

static Register arg_regs[] = {RDI, RSI, RDX, RCX, R8, R9};

static Register ret_regs[] = {RAX, RDX};

static Register scratch_regs[] = {
    R10, R11, RDI, RSI, RDX, RCX, R8, R9, RAX, // NOTE: Caller saved
    RBX, R12, R13, R14, R15,                   // NOTE: Callee saved
};

static uint32_t reg_flags[] = {
    [RAX] = RET_REG,      [RCX] = ARG_REG,      [RDX] = ARG_REG | RET_REG, [RBX] = CALLEE_SAVED, [RSP] = CALLEE_SAVED,
    [RBP] = CALLEE_SAVED, [RSI] = ARG_REG,      [RDI] = ARG_REG,           [R8] = ARG_REG,       [R9] = ARG_REG,
    [R12] = CALLEE_SAVED, [R13] = CALLEE_SAVED, [R14] = CALLEE_SAVED,      [R15] = CALLEE_SAVED};

#define MAX_OP_BYTE_SIZE 8
static const char* reg_names[MAX_OP_BYTE_SIZE + 1][REG_COUNT] = {
    [1] =
        {
            [RAX] = "al",
            [RCX] = "cl",
            [RDX] = "dl",
            [RBX] = "bl",
            [RSP] = "spl",
            [RBP] = "bpl",
            [RSI] = "sil",
            [RDI] = "dil",
            [R8] = "r8b",
            [R9] = "r9b",
            [R10] = "r10b",
            [R11] = "r11b",
            [R12] = "r12b",
            [R13] = "r13b",
            [R14] = "r14b",
            [R15] = "r15b",
        },
    [2] =
        {
            [RAX] = "ax",
            [RCX] = "cx",
            [RDX] = "dx",
            [RBX] = "bx",
            [RSP] = "sp",
            [RBP] = "bp",
            [RSI] = "si",
            [RDI] = "di",
            [R8] = "r8w",
            [R9] = "r9w",
            [R10] = "r10w",
            [R11] = "r11w",
            [R12] = "r12w",
            [R13] = "r13w",
            [R14] = "r14w",
            [R15] = "r15w",
        },
    [4] =
        {
            [RAX] = "eax",
            [RCX] = "ecx",
            [RDX] = "edx",
            [RBX] = "ebx",
            [RSP] = "esp",
            [RBP] = "ebp",
            [RSI] = "esi",
            [RDI] = "edi",
            [R8] = "r8d",
            [R9] = "r9d",
            [R10] = "r10d",
            [R11] = "r11d",
            [R12] = "r12d",
            [R13] = "r13d",
            [R14] = "r14d",
            [R15] = "r15d",
        },
    [8] =
        {
            [RAX] = "rax",
            [RCX] = "rcx",
            [RDX] = "rdx",
            [RBX] = "rbx",
            [RSP] = "rsp",
            [RBP] = "rbp",
            [RSI] = "rsi",
            [RDI] = "rdi",
            [R8] = "r8",
            [R9] = "r9",
            [R10] = "r10",
            [R11] = "r11",
            [R12] = "r12",
            [R13] = "r13",
            [R14] = "r14",
            [R15] = "r15",
        },
};

static char op_suffix[MAX_OP_BYTE_SIZE + 1] = {[1] = 'b', [2] = 'w', [4] = 'l', [8] = 'q'};

#define TMP_INST_BUF_LEN 64
struct Generator {
    FILE* out_fd;
    char* out_buf;
    char* data_buf;
    char tmp_inst_buf[TMP_INST_BUF_LEN];

    const char* curr_proc;
    Scope* curr_scope;

    uint32_t free_reg_mask;

    Allocator* arena;
};

typedef enum OperandKind {
    OPERAND_NONE,
    OPERAND_FRAME_OFFSET,
    OPERAND_REGISTER,
    OPERAND_IMMEDIATE,
    OPERAND_GLOBAL_VAR,
    OPERAND_PROC,
} OperandKind;

struct Operand {
    OperandKind kind;
    Type* type;

    union {
        int offset;
        Register reg;
        Scalar imm;
        const char* var;
        const char* proc;
    };
};

static Generator generator;

#define emit(f, ...) ftprint_char_array(&generator.out_buf, false, (f), ##__VA_ARGS__)
#define emit_data(f, ...) ftprint_char_array(&generator.data_buf, false, (f), ##__VA_ARGS__)

static void enter_gen_scope(Scope* scope);
static void exit_gen_scope();
static void gen_stmt(Stmt* stmt);
static void gen_expr(Expr* expr, Operand* dest);

static void print_reg_mask(unsigned i)
{
    unsigned reg_mask = generator.free_reg_mask;
    ftprint_out("%d: reg_mask: %b\n", i, reg_mask);
}

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

static void operand_from_sym(Operand* operand, Symbol* sym)
{
    switch (sym->kind)
    {
        case SYMBOL_VAR:
        {
            if (sym->is_local)
            {
                operand->kind = OPERAND_FRAME_OFFSET;
                operand->type = sym->type;
                operand->offset = sym->offset;
            }
            else
            {
                operand->kind = OPERAND_GLOBAL_VAR;
                operand->type = sym->type;
                operand->var = sym->name;
            }
            break;
        }
        case SYMBOL_PROC:
            operand->kind = OPERAND_PROC;
            operand->type = sym->type;
            operand->proc = sym->name;
            break;
        default:
            ftprint_err("INTERNAL ERROR: Cannot make operand from symbol kind %d\n", sym->kind);
            assert(0);
            break;
    }
}

static const char* add_inst(unsigned size)
{
    switch (size)
    {
        case 1:
            return "addb";
        case 2:
            return "addw";
        case 4:
            return "addl";
        case 8:
            return "addq";
        default:
            ftprint_err("INTERNAL ERROR: unsupported add instruction size: %u\n", size);
            assert(0);
            break;
    }

    return NULL;
}

static const char* mov_inst(unsigned size)
{
    switch (size)
    {
        case 1:
            return "movb";
        case 2:
            return "movw";
        case 4:
            return "movl";
        case 8:
            return "movq";
        default:
            ftprint_err("INTERNAL ERROR: unsupported mov instruction size: %u\n", size);
            assert(0);
            break;
    }

    return NULL;
}

static size_t movs_inst(char* buf, size_t len, unsigned op1_size, unsigned op2_size)
{
    char op1_suffix = op_suffix[op1_size];
    char op2_suffix = op_suffix[op2_size];

    if (op1_suffix && op2_suffix)
        return snprintf(buf, len, "movs%c%c", op1_suffix, op2_suffix);

    return snprintf(buf, len, "mov");
}

static bool try_alloc_reg(Register reg)
{
    unsigned reg_flag = (1 << reg);
    bool is_free = generator.free_reg_mask & reg_flag;

    if (is_free)
    {
        ftprint_out("Allocating reg %s\n", reg_names[8][reg]);
        generator.free_reg_mask &= ~reg_flag;
    }

    return is_free;
}

static void alloc_reg(Register reg)
{
    ftprint_out("Allocating reg %s\n", reg_names[8][reg]);
    generator.free_reg_mask &= ~(1 << reg);
}

static void free_reg(Register reg)
{
    ftprint_out("Freeing reg %s\n", reg_names[8][reg]);
    generator.free_reg_mask |= 1 << reg;
}

static unsigned init_regs()
{
    size_t num_scratch_regs = sizeof(scratch_regs) / sizeof(Register);

    for (size_t i = 0; i < num_scratch_regs; i += 1)
        free_reg(scratch_regs[i]);

    return generator.free_reg_mask;
}

static Register next_reg()
{
    Register reg = REG_INVALID;
    size_t num_regs = sizeof(scratch_regs) / sizeof(Register);

    for (size_t i = 0; i < num_regs; i += 1)
    {
        unsigned reg_flag = (1 << scratch_regs[i]);
        bool is_free = generator.free_reg_mask & reg_flag;

        if (is_free)
        {
            reg = scratch_regs[i];
            break;
        }
    }

    if (reg != REG_INVALID)
    {
        alloc_reg(reg);
    }
    else
    {
        // TODO: Spill registers
        ftprint_err("Out of free registers. TODO: spill registers\n");
        assert(0);
    }

    // TODO: Keep track of caller or callee-saved regs that we need to spill

    return reg;
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
    if (operand->kind == OPERAND_REGISTER)
        free_reg(operand->reg);
}

static void emit_operand_to_reg(Operand* operand, Register reg)
{
    size_t op_size = operand->type->size;
    const char* dst_reg_name = reg_names[op_size][reg];

    switch (operand->kind)
    {
        case OPERAND_IMMEDIATE:
        {
            char* movs_buf = generator.tmp_inst_buf;

            movs_inst(movs_buf, TMP_INST_BUF_LEN, 0, op_size);
            emit("    %s $%d, %%%s\n", movs_buf, operand->imm.as_int.i, dst_reg_name);
            break;
        }
        case OPERAND_FRAME_OFFSET:
            emit("    %s %d(%%rbp), %%%s\n", mov_inst(op_size), operand->offset, dst_reg_name);
            break;
        case OPERAND_GLOBAL_VAR:
            emit("    %s %s(%%rip), %%%s\n", mov_inst(op_size), operand->var, dst_reg_name);
            break;
        case OPERAND_REGISTER:
            if (operand->reg != reg)
                emit("    %s %%%s, %%%s\n", mov_inst(op_size), reg_names[op_size][operand->reg], dst_reg_name);

            break;
        default:
            ftprint_err("INTERNAL ERROR: Unexpected operand type %d\n", operand->kind);
            assert(0);
            break;
    }
}

static void ensure_operand_in_reg(Operand* operand)
{
    if (operand->kind != OPERAND_REGISTER)
    {
        Register reg = next_reg();
        emit_operand_to_reg(operand, reg);

        operand->kind = OPERAND_REGISTER;
        operand->reg = reg;
    }
}

static void emit_var_assign(Operand* var_op, Operand* rhs_op)
{
    size_t var_size = var_op->type->size;

    if (var_op->kind == OPERAND_FRAME_OFFSET)
    {
        int var_offset = var_op->offset;

        if (rhs_op->kind == OPERAND_IMMEDIATE)
        {
            emit("    %s $%d, %d(%%rbp)\n", mov_inst(var_size), rhs_op->imm.as_int.i, var_offset);
        }
        else
        {
            ensure_operand_in_reg(rhs_op);
            emit("    %s %%%s, %d(%%rbp)\n", mov_inst(var_size), reg_names[rhs_op->type->size][rhs_op->reg],
                 var_offset);
        }
    }
    else if (var_op->kind == OPERAND_GLOBAL_VAR)
    {
        const char* var_name = var_op->var;

        if (rhs_op->kind == OPERAND_IMMEDIATE)
        {
            emit("    %s $%d, %s(%%rip)\n", mov_inst(var_size), rhs_op->imm.as_int.i, var_name);
        }
        else
        {
            ensure_operand_in_reg(rhs_op);
            emit("    %s %%%s, %s(%%rip)\n", mov_inst(var_size), reg_names[rhs_op->type->size][rhs_op->reg], var_name);
        }
    }
    else
    {
        ftprint_err("INTERNAL ERROR: Do not yet support indirect assignment\n");
        assert(0);
    }
}

static void emit_add(Type* type, Operand* src, Operand* dst)
{
    assert(src->type == dst->type);
    assert(type == dst->type);
    size_t size = type->size;

    if (dst->kind == OPERAND_IMMEDIATE && src->kind == OPERAND_IMMEDIATE)
    {
        // NOTE: THIS SHOULDN'T happen because resolver should have already done constant folding.
        dst->imm.as_int.i += src->imm.as_int.i;
    }
    else if (dst->kind == OPERAND_IMMEDIATE)
    {
        // Add into the src register.
        ensure_operand_in_reg(src);
        emit("    %s $%d, %%%s\n", add_inst(size), dst->imm.as_int.i, reg_names[src->type->size][src->reg]);

        // Steal src operand's register.
        dst->kind = OPERAND_REGISTER;
        dst->type = type;
        dst->reg = src->reg;
        src->kind = OPERAND_NONE;
    }
    else
    {
        ensure_operand_in_reg(dst);

        if (src->kind == OPERAND_IMMEDIATE)
        {
            emit("    %s $%d, %%%s\n", add_inst(size), src->imm.as_int.i, reg_names[dst->type->size][dst->reg]);
        }
        else if (src->kind == OPERAND_FRAME_OFFSET)
        {
            emit("    %s %d(%%rbp), %%%s\n", add_inst(size), src->offset, reg_names[dst->type->size][dst->reg]);
        }
        else if (src->kind == OPERAND_GLOBAL_VAR)
        {
            emit("    %s %s(%%rip), %%%s\n", add_inst(size), src->var, reg_names[dst->type->size][dst->reg]);
        }
        else if (src->kind == OPERAND_REGISTER)
        {
            emit("    %s %%%s, %%%s\n", add_inst(size), reg_names[src->type->size][src->reg],
                 reg_names[dst->type->size][dst->reg]);
        }
        else
        {
            ftprint_err("INTERNAL ERROR: Unexpected src operand kind %d while generating add inst\n", src->kind);
            assert(0);
        }
    }
}

static void gen_expr_binary(ExprBinary* expr, Operand* dest)
{
    switch (expr->op)
    {
        case TKN_PLUS:
        {
            Operand src = {0};

            gen_expr(expr->left, dest);
            gen_expr(expr->right, &src);

            emit_add(expr->super.type, &src, dest);

            free_operand(&src);
            break;
        }
        default:
            ftprint_err("INTERNAL ERROR: Unsupported binary op %d during code generation\n", expr->op);
            assert(0);
            break;
    }
}

static void gen_expr_ident(ExprIdent* eident, Operand* dest)
{
    Symbol* sym = lookup_symbol(generator.curr_scope, eident->name);

    if (sym->kind == SYMBOL_VAR)
    {
        if (sym->is_local)
        {
            dest->kind = OPERAND_FRAME_OFFSET;
            dest->type = sym->type;
            dest->offset = sym->offset;
        }
        else
        {
            dest->kind = OPERAND_GLOBAL_VAR;
            dest->type = sym->type;
            dest->var = sym->name;
        }
    }
    else if (sym->kind == SYMBOL_PROC)
    {
        dest->kind = OPERAND_PROC;
        dest->type = sym->type;
        dest->proc = sym->name;
    }
    else
    {
        ftprint_err("INTERNAL ERROR: Unexpected symbol kind %d during code generation for ident expr\n", sym->kind);
        assert(0);
    }
}

static void gen_expr_call(ExprCall* ecall, Operand* dest)
{
    //
    // Generate procedure arguments.
    //
    AllocatorState mem_state = allocator_get_state(generator.arena);
    Operand* arg_ops = alloc_array(generator.arena, Operand, ecall->num_args, false);
    List* head = &ecall->args;
    size_t arg_index = 0;

    for (List* it = head->next; it != head; it = it->next)
    {
        ProcCallArg* call_arg = list_entry(it, ProcCallArg, lnode);

        // TODO: Optimization: pass preferred reg to gen_expr
        gen_expr(call_arg->expr, &arg_ops[arg_index]);

        if (arg_ops[arg_index].kind == OPERAND_REGISTER)
        {
            if (arg_ops[arg_index].reg != arg_regs[arg_index])
            {
                Register old_reg = arg_ops[arg_index].reg;

                if (!try_alloc_reg(arg_regs[arg_index]))
                {
                    ftprint_err("Failed to allocate arg register %d\n", arg_regs[arg_index]);
                    assert(0);
                }

                emit_operand_to_reg(&arg_ops[arg_index], arg_regs[arg_index]);
                free_reg(old_reg);
            }
        }
        else
        {
            emit_operand_to_reg(&arg_ops[arg_index], arg_regs[arg_index]);
        }

        free_operand(&arg_ops[arg_index]);

        arg_index += 1;
    }

    allocator_restore_state(mem_state);

    //
    // Generate procedure pointer/name expr.
    //
    Operand proc_op = {0};

    gen_expr(ecall->proc, &proc_op);

    if (proc_op.kind == OPERAND_PROC)
    {
        emit("    call %s\n", proc_op.proc);
    }
    else
    {
        assert(proc_op.kind == OPERAND_FRAME_OFFSET || proc_op.kind == OPERAND_GLOBAL_VAR);
        ensure_operand_in_reg(&proc_op);

        emit("    call *%%%s\n", reg_names[proc_op.type->size][proc_op.reg]);
    }

    free_operand(&proc_op);

    //
    // Result is in RAX
    //
    if (try_alloc_reg(RAX))
    {
        dest->kind = OPERAND_REGISTER;
        dest->type = ecall->super.type;
        dest->reg = RAX;
    }
    else
    {
        assert(0);
    }
}

static void gen_expr(Expr* expr, Operand* dest)
{
    if (expr->is_const)
    {
        dest->kind = OPERAND_IMMEDIATE;
        dest->type = expr->type;
        dest->imm = expr->const_val;

        return;
    }

    switch (expr->kind)
    {
        case CST_ExprIdent:
            gen_expr_ident((ExprIdent*)expr, dest);
            break;
        case CST_ExprBinary:
            gen_expr_binary((ExprBinary*)expr, dest);
            break;
        case CST_ExprCall:
            gen_expr_call((ExprCall*)expr, dest);
            break;
        default:
            ftprint_err("Unsupported expr kind %d during code generation\n", expr->kind);
            assert(0);
            break;
    }
}

static void gen_stmt_return(StmtReturn* sreturn)
{
    Operand operand = {0};

    gen_expr(sreturn->expr, &operand);
    ensure_operand_in_reg(&operand);

    if (operand.reg != RAX)
    {
        size_t op_size = operand.type->size;

        // TODO: Ensure RAX is actually available. If not, spill
        if (!try_alloc_reg(RAX))
        {
            ftprint_err("INTERNAL ERROR: Cannot allocate %rax\n");
            assert(0);
        }

        // mov OP_REG, %rax
        emit("    %s %%%s, %%%s\n", mov_inst(op_size), reg_names[op_size][operand.reg], reg_names[op_size][RAX]);
        free_reg(RAX);
    }

    free_operand(&operand);
    emit("    jmp end.%s\n", generator.curr_proc);
}

static void gen_stmt_expr_assign(StmtExprAssign* seassign)
{
    switch (seassign->op_assign)
    {
        case TKN_ASSIGN:
        {
            Operand loperand = {0};
            Operand roperand = {0};

            gen_expr(seassign->left, &loperand);
            gen_expr(seassign->right, &roperand);

            emit_var_assign(&loperand, &roperand);

            free_operand(&loperand);
            free_operand(&roperand);
            break;
        }
        default:
            ftprint_err("INTERNAL ERROR: Unsupported assignment op %d during code generation\n", seassign->op_assign);
    }
}

static void gen_stmt_decl(StmtDecl* sdecl)
{
    assert(sdecl->decl->kind == CST_DeclVar);

    DeclVar* dvar = (DeclVar*)sdecl->decl;

    // This will only init the variable.
    if (dvar->init)
    {
        Operand rhs_op = {0};
        Operand lhs_op = {0};
        Symbol* sym = lookup_symbol(generator.curr_scope, dvar->name);

        assert(sym);
        gen_expr(dvar->init, &rhs_op);
        operand_from_sym(&lhs_op, sym);
        emit_var_assign(&lhs_op, &rhs_op);
        free_operand(&rhs_op);
    }
}

static void gen_stmt_block(StmtBlock* sblock)
{
    enter_gen_scope(sblock->scope);

    List* head = &sblock->stmts;
    List* it = head->next;

    while (it != head)
    {
        Stmt* s = list_entry(it, Stmt, lnode);

        gen_stmt(s);

        it = it->next;
    }

    exit_gen_scope();
}

static void gen_stmt(Stmt* stmt)
{
    switch (stmt->kind)
    {
        case CST_StmtBlock:
            gen_stmt_block((StmtBlock*)stmt);
            break;
        case CST_StmtReturn:
            gen_stmt_return((StmtReturn*)stmt);
            break;
        case CST_StmtDecl:
            gen_stmt_decl((StmtDecl*)stmt);
            break;
        case CST_StmtExprAssign:
            gen_stmt_expr_assign((StmtExprAssign*)stmt);
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
        List* it = head->next;

        while (it != head)
        {
            Symbol* sym = list_entry(it, Symbol, lnode);

            if (sym->kind == SYMBOL_VAR)
            {
                stack_size += sym->type->size;
                stack_size = ALIGN_UP(stack_size, sym->type->align);
                sym->offset = -stack_size;
            }

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //
    {
        List* head = &scope->children;
        List* it = head->next;
        size_t child_offset = stack_size;

        while (it != head)
        {
            Scope* child_scope = list_entry(it, Scope, lnode);
            size_t child_size = compute_scope_var_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, 16);
}

static size_t compute_proc_var_offsets(DeclProc* dproc)
{
    size_t stack_size = 0;
    unsigned arg_index = 0;
    unsigned stack_arg_offset = 0x10;

    List* head = &dproc->scope->sym_list;
    List* it = head->next;

    while (it != head)
    {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (sym->kind == SYMBOL_VAR)
        {
            Type* arg_type = sym->type;
            size_t arg_size = arg_type->size;
            size_t arg_align = arg_type->align;
            bool arg_in_reg = (arg_index < ARRAY_LEN(arg_regs)) && (arg_size <= MAX_OP_BYTE_SIZE);

            // Spill argument register onto the stack.
            if (arg_in_reg)
            {
                Register arg_reg = arg_regs[arg_index];

                stack_size += arg_size;
                stack_size = ALIGN_UP(stack_size, arg_align);
                sym->offset = -stack_size;

                emit("    %s %%%s, %d(%%rbp)\n", mov_inst(arg_size), reg_names[arg_size][arg_reg], sym->offset);

                arg_index += 1;
            }
            else
            {
                sym->offset = stack_arg_offset;
                stack_arg_offset += arg_size;
                stack_arg_offset = ALIGN_UP(stack_arg_offset, arg_align);
            }
        }

        it = it->next;
    }

    assert(dproc->body->kind == CST_StmtBlock);
    StmtBlock* proc_body = (StmtBlock*)dproc->body;

    return compute_scope_var_offsets(proc_body->scope, stack_size);
}

static void enter_gen_scope(Scope* scope)
{
    generator.curr_scope = scope;
}

static void exit_gen_scope()
{
    generator.curr_scope = generator.curr_scope->parent;
}

static void enter_proc(DeclProc* dproc)
{
    generator.curr_proc = dproc->name;

    enter_gen_scope(dproc->scope);
}

static void exit_proc()
{
    generator.curr_proc = NULL;

    exit_gen_scope();
}

static void gen_proc(Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    enter_proc(dproc);

    emit("\n");
    emit(".text\n");
    emit(".globl %s\n", sym->name);
    emit("%s:\n", sym->name);

    emit("    push %%rbp\n");
    emit("    movq %%rsp, %%rbp\n");

    size_t stack_size = compute_proc_var_offsets(dproc);

    if (stack_size)
        emit("    subq $%d, %%rsp\n", stack_size);

    gen_stmt(dproc->body);

    emit("\n    end.%s:\n", sym->name);
    emit("    leave\n");
    emit("    ret\n");

    exit_proc();
}

static void gen_global_scope(Scope* scope)
{
    enter_gen_scope(scope);

    //
    // Generate global variables
    //
    List* head = &scope->sym_list;
    List* it = head->next;

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

bool gen_gasm(Allocator* arena, Scope* scope, const char* output_file)
{
    generator.arena = arena;
    generator.out_buf = array_create(NULL, char, 512);
    generator.data_buf = array_create(NULL, char, 512);
    generator.out_fd = fopen(output_file, "w");

    if (!generator.out_fd)
    {
        ftprint_err("Failed to open output file `%s`\n", output_file);
        return false;
    }

    unsigned init_reg_mask = init_regs();

    ftprint_out("INIT REG MASK: %b\n", init_reg_mask);
    emit("# Generated by Nibble compiler\n");
    gen_global_scope(scope);

    unsigned final_reg_mask = generator.free_reg_mask;
    ftprint_out("FINAL REG MASK: %b\n", final_reg_mask);

    array_push(generator.out_buf, '\0');
    ftprint_file(generator.out_fd, false, "%s", generator.out_buf);

    array_free(generator.out_buf);
    array_free(generator.data_buf);
    fclose(generator.out_fd);

    return true;
}
