#include "bytecode.h"

IR_Instr** IR_get_bucket_instr(BucketList* bucket_list, size_t index)
{
    return (IR_Instr**)bucket_list_get_elem(bucket_list, index);
}

IR_Instr** IR_add_bucket_instr(BucketList* bucket_list, Allocator* arena, IR_Instr* instr)
{
    return (IR_Instr**)bucket_list_add_elem(bucket_list, arena, instr);
}

IR_Instr* IR_new_instr(Allocator* arena, IR_InstrKind kind, s16 option, s32 r, IR_InstrArg a, IR_InstrArg b)
{
    IR_Instr* instr = alloc_type(arena, IR_Instr, false);

    instr->kind = kind;
    instr->option = option;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    return instr;
}

typedef struct IR_SIBDAddr {
    s32 base_reg;
    s32 index_reg;
    s16 scale;
    u64 disp;
} IR_SIBDAddr;

typedef enum IR_OperandKind {
    IR_OPERAND_NONE,
    IR_OPERAND_IMM,
    IR_OPERAND_REG,
    IR_OPERAND_SIBD_ADDR,
    IR_OPERAND_DEREF_ADDR,
    IR_OPERAND_VAR,
    IR_OPERAND_PROC,
} IR_OperandKind;

typedef struct IR_Operand {
    IR_OperandKind kind;
    Type* type;

    union {
        Scalar imm;
        s32 reg;
        IR_SIBDAddr addr;
        Symbol* sym;
    };
} IR_Operand;

static s32 IR_next_reg(IR_Builder* builder)
{
    builder->num_regs += 1;
    return builder->num_regs;
}

static void IR_execute_deref(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEREF_ADDR);

    s32 dst_reg = IR_next_reg(builder);

    IR_emit_load(builder, dst_reg, operand);

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_execute_lea(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_SIBD_ADDR);

    // The operand currently holds a memory address.
    // This function executes the "load-effective-address" call into the one of the registers that held the address.
    SIBDAddr addr = operand->addr;

    bool has_base = addr.base_reg < IR_REG_COUNT;
    bool has_index = addr.scale && (addr.index_reg < IR_REG_COUNT);
    bool has_disp = addr.disp != 0;
    s32 dst_reg;

    assert(has_base || has_index);

    if (has_base && !has_index && !has_disp)
    {
        // No need to emit any instructions. Just keep address in base register.
        dst_reg = operand->addr.base_reg;
    }
    else
    {
        s32 dst_reg = IR_next_reg(builder);

        IR_emit_laddr(builder, dst_reg, operand);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_ensure_execute_deref(IR_Builder* builder, IR_Operand* operand)
{
    if (operand->kind == IR_OPERAND_DEREF_ADDR)
        IR_execute_deref(builder, operand);
}

static void IR_ensure_execute_lea(IR_Builder* builder, IR_Operand* operand)
{
    if (operand->kind == IR_OPERAND_SIBD_ADDR)
        IR_execute_lea(builder, operand);
}

static void IR_commit_indirections(IR_Builder* builder, IR_Operand* operand)
{
    IR_ensure_execute_deref(builder, operand);
    IR_ensure_execute_lea(builder, operand);
}

static void IR_ensure_operand_in_reg(IR_Builder* builder, IR_Operand* operand, bool commit_ptr)
{
    if (commit_ptr && (operand->kind == IR_OPERAND_SIBD_ADDR))
    {
        IR_execute_lea(builder, operand);
    }
    if (operand->kind == IR_OPERAND_DEREF_ADDR)
    {
        IR_execute_deref(builder, operand);
    }
    else if (operand->kind != IR_OPERAND_REG)
    {
        assert(operand->kind == IR_OPERAND_VAR);

        if (!commit_ptr && (operand->type->kind == TYPE_PTR))
        {
            if (operand->kind != IR_OPERAND_SIBD_ADDR)
            {
                Register base_reg = IR_next_reg(builder);
                IR_emit_load(builder, base_reg, operand->sym);

                operand->kind = IR_OPERAND_SIBD_ADDR;
                operand->addr.base_reg = base_reg;
                operand->addr.index_reg = -1;
                operand->addr.scale = 0;
                operand->addr.disp = 0;
            }
        }
        else
        {
            Register reg = IR_next_reg(builder);
            IR_emit_load(builder, reg, operand->sym);

            operand->kind = IR_OPERAND_REG;
            operand->reg = reg;
        }
    }
}

//////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////

void IR_emit_expr_ident(IR_Builder* builder, ExprIdent* eident, IR_Operand* dst)
{
    Symbol* sym = lookup_symbol(builder->curr_scope, eident->name);

    if (sym->kind == SYMBOL_VAR)
    {
        dst->kind = IR_OPERAND_VAR;
        dst->type = sym->type;
        dst->sym = sym;
    }
    else if (sym->kind == SYMBOL_PROC)
    {
        dst->kind = IR_OPERAND_PROC;
        dst->type = sym->type;
        dst->sym = sym;
    }
    else
    {
        ftprint_err("INTERNAL ERROR: Unexpected symbol kind %d during code generation for ident expr\n", sym->kind);
        assert(0);
    }
}

void IR_emit_ptr_int_add(IR_Builder* builder, IR_Operand* ptr_op, IR_Operand* int_op, bool add)
{
    u64 base_size = ptr_op->type->as_ptr.base->size;

    IR_ensure_operand_in_reg(builder, ptr_op, false);

    if (int_op->kind == IR_OPERAND_IMM)
    {
        if (add)
            ptr_op->addr.disp += base_size * int_op->imm.as_int._u64;
        else
            ptr_op->addr.disp -= base_size * int_op->imm.as_int._u64;
    }
    else
    {
        if (ptr_op->addr.scale)
        {
            s32 index_reg = IR_next_reg(builder);
            IR_InstrArg a = {.kind = IR_ARG_REG, .reg0 = ptr_op->addr.index_reg, .reg1 = -1};
            IR_InstrArg b = {0};

            IR_ensure_operand_in_reg(builder, int_op, true);
            IR_arg_from_operand(&b, int_op);

            if (add)
                IR_emit_add(builder, IR_TYPE_INT64, index_reg, a, b);
            else
                IR_emit_sub(builder, IR_TYPE_INT64, index_reg, a, b);

            ptr_op->addr.index_reg = index_reg;
        }
        else
        {
            IR_ensure_operand_in_reg(builder, int_op, true);
            
            if (!add)
            {
                s32 index_reg = IR_next_reg(builder);
                IR_Type type = IR_get_type(int_op->type);
                IR_InstrArg a = {.kind = IR_ARG_REG, .reg0 = int_op->reg, .reg1 = -1};

                IR_emit_neg(builder, type, index_reg, a);

                int_op->reg = index_reg;
            }

            ptr_op->addr.scale = base_size;
            ptr_op->addr.index_reg = int_op->reg;
        }
    }
}

void IR_emit_expr_binary(IR_Builder* builder, ExprBinary* expr, IR_Operand* dst)
{
    IR_Operand left = {0};
    IR_Operand right = {0};

    switch (expr->op)
    {
        case TKN_PLUS:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            bool left_is_ptr = left.type->kind == TYPE_PTR;
            bool right_is_ptr = right.type->kind == TYPE_PTR;

            if (left_is_ptr)
            {
                IR_emit_ptr_int_add(builder, dst, &left, &right, true);
            }
            else if (right_is_ptr)
            {
                IR_emit_ptr_int_add(builder, dst, &right, &left, true);
            }
            else
            {
                assert(left.type == right.type);
                assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

                dst->kind = IR_OPERAND_REG;
                dst->type = left.type;
                dst->reg = IR_next_reg(builder);

                IR_ensure_execute_deref(builder, &left);
                IR_ensure_execute_deref(builder, &right);
                IR_emit_add(builder, dst, &left, &right);
            }

            IR_free_operand_regs(builder, &left);
            IR_free_operand_regs(builder, &right);
            break;
        }
        case TKN_MINUS:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            bool left_is_ptr = left.type->kind == TYPE_PTR;
            bool right_is_ptr = right.type->kind == TYPE_PTR;

            // ptr - int => ptr
            if (left_is_ptr && !right_is_ptr)
            {
                IR_emit_ptr_int_add(builder, dst, &left, &right, false);
            }
            // ptr - ptr => s64
            else if (left_is_ptr && right_is_ptr)
            {
                Type* result_type = expr->super.type;
                u64 base_size = left.type->as_ptr.base->size;
                s32 base_size_log2 = (s32)clp2(base_size);

                dst->kind = IR_OPERAND_REG;
                dst->type = result_type;
                dst->reg = IR_next_reg(builder);

                IR_ensure_execute_lea(builder, &left);
                IR_ensure_execute_lea(builder, &right);
                IR_emit_sub(builder, dst, &left, &right);

                if (base_size_log2)
                    IR_emit_rshift(builder, dst, dst, base_size_log2);
            }
            // int - int => int
            else
            {
                assert(left.type == right.type);
                assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

                dst->kind = IR_OPERAND_REG;
                dst->type = left.type;
                dst->reg = IR_next_reg(builder);

                IR_ensure_execute_deref(builder, &left);
                IR_ensure_execute_deref(builder, &right);
                IR_emit_sub(builder, dst, &left, &right);
            }

            IR_free_operand_regs(builder, &left);
            IR_free_operand_regs(builder, &right);
            break;
        }
    }
}

void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst)
{
    if (expr->is_const)
    {
        dst->kind = kind = IR_OPERAND_IMM;
        dst->type = expr->type;
        dst->imm = expr->const_val;
    }

    switch (expr->kind)
    {
        case CST_ExprIdent:
            IR_emit_expr_ident(builder, (ExprIdent*)expr, dst);
            break;
        case CST_ExprCall:
            IR_emit_expr_call(builder, (ExprCall*)expr, dst);
            break;
        case CST_ExprCast:
            IR_emit_expr_cast(builder, (ExprCast*)expr, dst);
            break;
        case CST_ExprBinary:
            IR_emit_expr_binary(builder, (ExprBinary*)expr, dst);
            break;
        case CST_ExprUnary:
            IR_emit_expr_unary(builder, (ExprUnary*)expr, dst);
            break;
        case CST_ExprIndex:
            IR_emit_expr_index(builder, (ExprIndex*)expr, dst);
            break;
        default:
            ftprint_err("Unsupported expr kind %d during code generation\n", expr->kind);
            assert(0);
            break;
    }
}

