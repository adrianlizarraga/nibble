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

void IR_free_reg(IR_Builder* builder, IR_RegID reg)
{
    IR_set_reg(&builder->free_regs, reg);
}

void IR_alloc_reg(IR_Builder* builder, IR_RegID reg)
{
    IR_unset_reg(&builder->free_regs, reg);
}

bool IR_try_alloc_reg(IR_Builder* builder, IR_RegID reg)
{
    bool is_free = IR_is_reg_set(builder->free_regs, reg);

    if (is_free)
        IR_alloc_reg(builder, reg);

    return is_free;
}

u32 IR_init_free_regs(IR_Builder* builder)
{
    for (size_t i = 0; i < IR_NUM_TMP_REGS; i += 1)
        IR_free_reg(builder, IR_tmp_regs[i]);

    return builder->free_regs;
}

IR_RegID IR_next_reg(IR_Builder* builder)
{
    IR_RegID reg = IR_REG_INVALID;
    int bit_index = ntz(builder->free_regs);

    assert(bit_index <= IR_REG5);

    reg = (IR_RegID)bit_index;

    IR_alloc_reg(reg);

    return reg;
}

void IR_free_op(IR_Operand* op)
{
    if (op->kind == IR_OPERAND_REG)
        IR_free_reg(op->_reg.reg);
}

void IR_emit_op_to_reg(IR_Operand* src_op, IR_RegID reg, IR_Operand* dst_op)
{
    switch (src_op->kind)
    {
        case IR_OPERAND_IMM:
        {
            dst_op->kind = IR_OPERAND_REG;
            dst_op->flags &= ~IR_OPERAND_IS_L_VALUE;
            dst_op->_reg.reg = reg;
            dst_op->_reg.type = src_op->_imm.type;

            IR_push_instr(builder, IR_OPCODE_LOADI, *src_op, *dst_op);

            break;
        }
        case IR_OPERAND_VAR:
        {
            dst_op->kind = IR_OPERAND_REG;
            dst_op->flags &= ~IR_OPERAND_IS_L_VALUE;
            dst_op->_reg.reg = reg;
            dst_op->_reg.type = src_op->_var.type;

            IR_push_instr(builder, IR_OPCODE_LOAD, *src_op, *dst_op);

            break;
        }
        case IR_OPERAND_REG:
        {
            if (src_op->_reg.reg != reg)
            {
                dst_op->kind = IR_OPERAND_REG;
                dst_op->flags &= ~IR_OPERAND_IS_L_VALUE;
                dst_op->_reg.reg = reg;
                dst_op->_reg.type = src_op->_reg.type;

                IR_push_instr(builder, IR_OPCODE_R2R, *src_op, *dst_op);
            }

            break;
        }
        default:
            ftprint_err("INTERNAL ERROR: Unexpected operand type %d\n", src_op->kind);
            assert(0);
            break;
    }
}

void IR_ensure_op_in_reg(IR_Builder* builder, IR_Operand* op)
{
    if (op->kind != IR_OPERAND_REG)
    {
        IR_RegID reg = IR_next_reg(builder);
        IR_Operand dst_op = {0};

        IR_emit_op_to_reg(op, reg, &dst_op);

        *op = dst_op;
    }
}

IR_Instr* IR_push_instr(IR_Builder* builder, IR_Opcode opcode, IR_Operand* src_op, IR_Operand* dst_op)
{
    IR_Instr* instr = IR_new_instr(builder->arena, opcode, *src_op, *dst_op);

    IR_add_bucket_instr(builder->instr_bucket, instr);

    return instr;
}

static void IR_emit_binary_instr(IR_Builder* builder, IR_Opcode opcode, IR_Operand* src_op, IR_Operand* dst_op)
{
    IR_ensure_op_in_reg(dst_op);
    IR_push_instr(builder, opcode, src_op, dst_op);
}

void IR_emit_add(IR_Builder* builder, IR_Operand* src_op, IR_Operand* dst_op)
{
    if (dst_op->kind == IR_OPERAND_IMM && src_op->kind == IR_OPERAND_IMM)
    {
        dst_op->_imm.as_int._s32 += src_op->_imm.as_int._s32;
    }
    else if (dst_op->kind == IR_OPERAND_IMM)
    {
        // Add into the src register, so flip src and dst operands.
        IR_emit_binary_instr(builder, IR_OPCODE_ADD, dst_op, src_op);

        // Steal src operand's register.
        dst_op->kind = IR_OPERAND_REG;
        dst_op->flags &= ~IR_OPERAND_IS_L_VALUE;
        dst_op->_reg = src_op->_reg;

        src_op->kind = IR_OPERAND_NONE;
    }
    else
    {
        IR_emit_binary_instr(IR_OPCODE_ADD, src_op, dst_op);
    }
}

void IR_emit_sub(IR_Builder* builder, IR_Operand* src_op, IR_Operand* dst_op)
{
    if (dst_op->kind == IR_OPERAND_IMM && src_op->kind == IR_OPERAND_IMM)
    {
        dst_op->_imm.as_int._s32 -= src_op->_imm.as_int._s32;
    }
    else
    {
        IR_emit_binary_instr(IR_OPCODE_SUB, src_op, dst_op);
    }
}
