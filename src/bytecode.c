#include "bytecode.h"

IR_Instr** IR_get_bucket_instr(BucketList* bucket_list, size_t index)
{
    return (IR_Instr**)bucket_list_get_elem(bucket_list, index);
}

IR_Instr** IR_add_bucket_instr(BucketList* bucket_list, Allocator* arena, IR_Instr* instr)
{
    return (IR_Instr**)bucket_list_add_elem(bucket_list, arena, instr);
}

IR_Instr* IR_new_instr(Allocator* arena, IR_OpCode opcode, IR_Operand op_s, IR_Operand op_d)
{
    IR_Instr* instr = alloc_type(arena, IR_Instr, false);

    instr->opcode = opcode;
    instr->operand_s = op_s;
    instr->operand_d = op_d;

    return instr;
}

static int ntz(u32 x)
{
    //
    //  ntz() from Hacker's Delight 2nd edition, pg 108
    //  Calculates the number of trailing zeros.
    //

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

const char* IR_reg_names[IR_REG_COUNT] = {
    [IR_REG0] = "IR_REG0",
    [IR_REG1] = "IR_REG1",
    [IR_REG2] = "IR_REG2",
    [IR_REG3] = "IR_REG3",
    [IR_REG4] = "IR_REG4",
    [IR_REG5] = "IR_REG5",
    [IR_ARG_REG0] = "IR_ARG_REG0",
    [IR_ARG_REG1] = "IR_ARG_REG1",
    [IR_ARG_REG2] = "IR_ARG_REG2",
    [IR_ARG_REG3] = "IR_ARG_REG3",
    [IR_ARG_REG4] = "IR_ARG_REG4",
    [IR_ARG_REG5] = "IR_ARG_REG5",
    [IR_RET_REG0] = "IR_RET_REG0",
    [IR_RET_REG1] = "IR_RET_REG1",
    [IR_FP_REG] = "IR_FP_REG",
};

IR_RegID IR_arg_regs[IR_NUM_ARG_REGS] = {
    IR_ARG_REG0, IR_ARG_REG1, IR_ARG_REG2, IR_ARG_REG3, IR_ARG_REG4, IR_ARG_REG5
};

IR_RegID IR_ret_regs[IR_NUM_RET_REGS] = {
    IR_RET_REG0, IR_RET_REG1
};

IR_RegID IR_tmp_regs[IR_NUM_TMP_REGS] = {
    IR_REG0, IR_REG1, IR_REG2, IR_REG3, IR_REG4, IR_REG5
};

static void IR_set_reg(u32* reg_mask, IR_RegID reg)
{
    *reg_mask |= (1 << reg);
}

static void IR_unset_reg(u32* reg_mask, IR_RegID reg)
{
    *reg_mask &= ~(1 << reg);
}

static bool IR_is_reg_set(u32 reg_mask, IR_RegID reg)
{
    return reg_mask & (1 << reg);
}

static void IR_free_reg(IR_Builder* builder, IR_RegID reg)
{
    IR_set_reg(&builder->free_regs, reg);
}

static void IR_alloc_reg(IR_Builder* builder, IR_RegID reg)
{
    IR_unset_reg(&builder->free_regs, reg);
}

static bool IR_try_alloc_reg(IR_Builder* builder, IR_RegID reg)
{
    bool is_free = IR_is_reg_set(builder->free_regs, reg);

    if (is_free)
        IR_alloc_reg(builder, reg);

    return is_free;
}

static u32 IR_init_free_regs(IR_Builder* builder)
{
    for (size_t i = 0; i < IR_NUM_TMP_REGS; i += 1)
        IR_free_reg(builder, IR_tmp_regs[i]);

    return builder->free_regs;
}

static IR_RegID IR_next_reg(IR_Builder* builder)
{
    IR_RegID reg = IR_REG_INVALID;
    int bit_index = ntz(builder->free_regs);

    assert(bit_index <= IR_REG5);

    reg = (IR_RegID)bit_index;

    IR_alloc_reg(builder, reg);

    return reg;
}

static void IR_free_operand_regs(IR_Builder* builder, IR_Operand* operand)
{
    if (operand->kind == IR_OPERAND_REG)
    {
        IR_free_reg(builder, operand->reg);
    }
    else if ((operand->kind == IR_OPERAND_DEREF_ADDR) || (operand->kind == IR_OPERAND_SIBD_ADDR))
    {
        Register base_reg = operand->addr.base_reg;
        Register index_reg = operand->addr.index_reg;

        if (base_reg < IR_REG_COUNT && base_reg != IR_FP_REG)
            IR_free_reg(builder, base_reg);

        if (index_reg < IR_REG_COUNT && index_reg != IR_FP_REG)
            IR_free_reg(builder, index_reg);
    }
}

static void IR_execute_deref(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEREF_ADDR);

    // The operand currently holds the address it is supposed to derefence.
    // This function executes the dereference into the one of the registers that held the address.
    SIBDAddr addr = operand->addr;

    bool has_base = addr.base_reg < IR_REG_COUNT;
    bool has_index = addr.scale && (addr.index_reg < IR_REG_COUNT);

    assert(has_base || has_index);

    Register dst_reg = has_base ? addr.base_reg : addr.index_reg;

    if (dst_reg == IR_FP_REG)
        dst_reg = IR_next_reg(builder);

    IR_emit_load_var(builder, dst_reg, operand);

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;

    if (has_base && (addr.base_reg != dst_reg) && (addr.base_reg != IR_FP_REG))
        IR_free_reg(builder, addr.base_reg);

    if (has_index && (addr.index_reg != dst_reg) && (addr.index_reg != IR_FP_REG))
        IR_free_reg(builder, addr.index_reg);
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
    Register dst_reg;

    assert(has_base || has_index);

    if (has_base && !has_index && !has_disp)
    {
        // No need to emit any instructions. Just keep address in base register.
        dst_reg = operand->addr.base_reg;
    }
    else
    {
        dst_reg = has_base ? addr.base_reg : addr.index_reg;

        if (dst_reg == IR_FP_REG)
            dst_reg = next_reg(builder);

        IR_emit_load_addr(builder, dst_reg, operand);

        if (has_base && (addr.base_reg != dst_reg) && (addr.base_reg != IR_FP_REG))
            IR_free_reg(builder, addr.base_reg);

        if (has_index && (addr.index_reg != dst_reg) && (addr.index_reg != IR_FP_REG))
            IR_free_reg(builder, addr.index_reg);
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

static void IR_emit_operand_to_reg(IR_Builder* builder, IR_Operand* operand, Register reg, size_t reg_size)
{
    switch (operand->kind)
    {
        case IR_OPERAND_IMM:
            IR_emit_load_imm(builder, operand->type, reg, operand->imm);
            break;
        case IR_OPERAND_VAR:
            IR_emit_load_var(builder, operand->type, reg, operand->sym);
            break;
        case IR_OPERAND_REG:
            if (operand->reg != reg)
                IR_emit_r2r(builder, operand->type, reg, operand->reg);

            break;
        default:
            ftprint_err("INTERNAL ERROR: Unexpected operand type %d\n", operand->kind);
            assert(0);
            break;
    }
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
        if (!commit_ptr && (operand->type->kind == TYPE_PTR))
        {
            if (operand->kind != IR_OPERAND_SIBD_ADDR)
            {
                Register base_reg = IR_next_reg(builder);
                IR_emit_operand_to_reg(builder, operand, base_reg, operand->type->size);

                operand->kind = IR_OPERAND_SIBD_ADDR;
                operand->addr.base_reg = base_reg;
                operand->addr.index_reg = IR_REG_INVALID;
                operand->addr.scale = 0;
                operand->addr.disp = 0;
            }
        }
        else
        {
            Register reg = IR_next_reg(builder);
            IR_emit_operand_to_reg(builder, operand, reg, operand->type->size);

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

void IR_emit_ptr_int_add(IR_Builder* builder, IR_Operand* dst, IR_Operand* ptr_op, IR_Operand* int_op, bool add)
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
            IR_Operand index_op = {.kind = IR_OPERAND_REG, .type = type_u64, .reg = ptr_op->addr.index_reg};

            IR_ensure_execute_deref(builder, int_op);

            if (add)
                IR_emit_add(builder, &index_op, &index_op, int_op);
            else
                IR_emit_sub(builder, &index_op, &index_op, int_op);
        }
        else
        {
            IR_ensure_operand_in_reg(builder, int_op, true);
            
            if (!add)
                IR_emit_neg(builder, int_op, int_op);

            ptr_op->addr.scale = base_size;
            ptr_op->addr.index_reg = int_op->reg;

            int_op->kind = IR_OPERAND_NONE; // Steal register.
        }
    }

    *dst = *ptr_op;
    ptr_op->kind = IR_OPERAND_NONE; // Steal registers
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

