#include "ast.h"
#include "bytecode.h"
#include "print_ir.h"
#include "stream.h"

static const IR_ConditionKind ir_opposite_cond[] = {
    [IR_COND_U_LT] = IR_COND_U_GTEQ, [IR_COND_S_LT] = IR_COND_S_GTEQ, [IR_COND_U_LTEQ] = IR_COND_U_GT,
    [IR_COND_S_LTEQ] = IR_COND_S_GT, [IR_COND_U_GT] = IR_COND_U_LTEQ, [IR_COND_S_GT] = IR_COND_S_LTEQ,
    [IR_COND_U_GTEQ] = IR_COND_U_LT, [IR_COND_S_GTEQ] = IR_COND_S_LT, [IR_COND_EQ] = IR_COND_NEQ,
    [IR_COND_NEQ] = IR_COND_EQ,
};

typedef enum IR_OperandKind {
    IR_OPERAND_NONE,
    IR_OPERAND_IMM,
    IR_OPERAND_REG,
    IR_OPERAND_SIBD_ADDR,
    IR_OPERAND_DEREF_ADDR,
    IR_OPERAND_DEFERRED_CMP,
    IR_OPERAND_VAR,
    IR_OPERAND_PROC,
} IR_OperandKind;

typedef struct IR_DeferredJmpcc {
    IR_ConditionKind cond;
    bool result;
    IR_Instr* jmp;

    struct IR_DeferredJmpcc* next;
} IR_DeferredJmpcc;

typedef struct IR_DeferredCmp {
    IR_DeferredJmpcc* first_sc_jmp;
    IR_DeferredJmpcc* last_sc_jmp;
    IR_DeferredJmpcc final_jmp;
} IR_DeferredCmp;

typedef struct IR_Operand {
    IR_OperandKind kind;
    Type* type;

    union {
        Scalar imm;
        IR_Reg reg;
        IR_SIBDAddr addr;
        Symbol* sym;
        IR_DeferredCmp cmp;
    };
} IR_Operand;

typedef struct IR_Builder {
    Allocator* arena;
    Allocator* tmp_arena;
    IR_Module* module;
    Symbol* curr_proc;
    Scope* curr_scope;

    IR_DeferredJmpcc* sc_jmp_freelist;
    u32 free_regs;
} IR_Builder;

//  Calculates the number of trailing zeros (bitscan).
//  ntz() from Hacker's Delight 2nd edition, pg 108
static int num_trailing_zeros(u32 x)
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

static void IR_set_reg(u32* reg_mask, IR_Reg reg)
{
    *reg_mask |= (1 << reg);
}

static void IR_unset_reg(u32* reg_mask, IR_Reg reg)
{
    *reg_mask &= ~(1 << reg);
}

static bool IR_is_reg_set(u32 reg_mask, IR_Reg reg)
{
    return reg_mask & (1 << reg);
}

static void IR_free_reg(IR_Builder* builder, IR_Reg reg)
{
    assert(reg < IR_REG_COUNT);
    assert(!IR_is_reg_set(builder->free_regs, reg));
    IR_set_reg(&builder->free_regs, reg);
}

static void IR_alloc_reg(IR_Builder* builder, IR_Reg reg)
{
    assert(reg < IR_REG_COUNT);
    assert(IR_is_reg_set(builder->free_regs, reg));
    IR_unset_reg(&builder->free_regs, reg);
}

static IR_Reg IR_next_reg(IR_Builder* builder)
{
    int bit_index = num_trailing_zeros(builder->free_regs);
    IR_Reg reg = (IR_Reg)bit_index;

    IR_alloc_reg(builder, reg);

    return reg;
}

static void IR_try_free_op_reg(IR_Builder* builder, IR_Operand* op)
{
    switch (op->kind)
    {
        case IR_OPERAND_REG:
            IR_free_reg(builder, op->reg);
            break;
        case IR_OPERAND_DEREF_ADDR:
        case IR_OPERAND_SIBD_ADDR:
        {
            IR_Reg base_reg = op->addr.base_reg;
            IR_Reg index_reg = op->addr.index_reg;

            if (base_reg < IR_REG_COUNT)
                IR_free_reg(builder, base_reg);

            if (index_reg < IR_REG_COUNT)
                IR_free_reg(builder, index_reg);

            break;
        }
        default:
            break;
    }
}

//////////////////////////////////////////////////////
//
//         Create IR instructions
//
//////////////////////////////////////////////////////
static void IR_add_instr(IR_Builder* builder, IR_Instr* instr)
{
    array_push(builder->curr_proc->as_proc.instrs, instr);
}

static IR_Instr* IR_new_instr(Allocator* arena, IR_InstrKind kind)
{
    IR_Instr* instr = alloc_type(arena, IR_Instr, true);
    instr->kind = kind;

    return instr;
}

static void IR_emit_instr_add(IR_Builder* builder, Type* type, IR_OpRM dst, IR_OpRMI src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_ADD);
    instr->_add.type = type;
    instr->_add.dst = dst;
    instr->_add.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sub(IR_Builder* builder, Type* type, IR_OpRM dst, IR_OpRMI src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SUB);
    instr->_sub.type = type;
    instr->_sub.dst = dst;
    instr->_sub.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_shr(IR_Builder* builder, Type* type, IR_OpRM dst, IR_OpRMI src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SHR);
    instr->_shr.type = type;
    instr->_shr.dst = dst;
    instr->_shr.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_neg(IR_Builder* builder, Type* type, IR_OpRM dst)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_NEG);
    instr->_neg.type = type;
    instr->_neg.dst = dst;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_not(IR_Builder* builder, Type* type, IR_OpRM dst)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_NOT);
    instr->_not.type = type;
    instr->_not.dst = dst;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_mov(IR_Builder* builder, Type* type, IR_Reg dst, IR_OpRI src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_MOV);
    instr->_mov.type = type;
    instr->_mov.dst = dst;
    instr->_mov.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_load_ptr(IR_Builder* builder, Type* type, IR_Reg dst, IR_SIBDAddr src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LOAD);
    instr->_load.type = type;
    instr->_load.dst = dst;
    instr->_load.src.kind = IR_MEM_ADDR_SIBD;
    instr->_load.src.sibd = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_load_var(IR_Builder* builder, Type* type, IR_Reg dst, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LOAD);
    instr->_load.type = type;
    instr->_load.dst = dst;
    instr->_load.src.kind = IR_MEM_ADDR_SYM;
    instr->_load.src.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_store_ptr(IR_Builder* builder, Type* type, IR_SIBDAddr dst, IR_OpRI src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_STORE);
    instr->_store.type = type;
    instr->_store.dst.kind = IR_MEM_ADDR_SIBD;
    instr->_store.dst.sibd = dst;
    instr->_store.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_store_var(IR_Builder* builder, Type* type, Symbol* dst, IR_OpRI src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_STORE);
    instr->_store.type = type;
    instr->_store.dst.kind = IR_MEM_ADDR_SYM;
    instr->_store.dst.sym = dst;
    instr->_store.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_laddr_ptr(IR_Builder* builder, IR_Reg dst, IR_SIBDAddr addr)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR);
    instr->_laddr.dst = dst;
    instr->_laddr.mem.kind = IR_MEM_ADDR_SIBD;
    instr->_laddr.mem.sibd = addr;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_laddr_var(IR_Builder* builder, IR_Reg dst, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR);
    instr->_laddr.dst = dst;
    instr->_laddr.mem.kind = IR_MEM_ADDR_SYM;
    instr->_laddr.mem.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_ret(IR_Builder* builder, Type* type, IR_Reg src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_RET);
    instr->_ret.type = type;
    instr->_ret.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_cmp(IR_Builder* builder, Type* type, IR_OpRM op1, IR_OpRMI op2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CMP);
    instr->_cmp.type = type;
    instr->_cmp.op1 = op1;
    instr->_cmp.op2 = op2;

    IR_add_instr(builder, instr);
}

static IR_Instr* IR_emit_instr_jmpcc(IR_Builder* builder, IR_ConditionKind cond, u32 jmp_target)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_JMPCC);
    instr->_jmpcc.cond = cond;
    instr->_jmpcc.jmp_target = jmp_target;

    IR_add_instr(builder, instr);

    return instr;
}

static IR_Instr* IR_emit_instr_jmp(IR_Builder* builder, u32 jmp_target)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_JMP);
    instr->_jmp.jmp_target = jmp_target;

    IR_add_instr(builder, instr);

    return instr;
}

static void IR_emit_instr_setcc(IR_Builder* builder, IR_ConditionKind cond, IR_OpRM dst)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SETCC);
    instr->_setcc.cond = cond;
    instr->_setcc.dst = dst;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_trunc(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_OpRM src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_TRUNC);
    instr->_trunc.dst_type = dst_type;
    instr->_trunc.dst = dst;
    instr->_trunc.src_type = src_type;
    instr->_trunc.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_zext(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_OpRM src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_ZEXT);
    instr->_zext.dst_type = dst_type;
    instr->_zext.dst = dst;
    instr->_zext.src_type = src_type;
    instr->_zext.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sext(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_OpRM src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SEXT);
    instr->_sext.dst_type = dst_type;
    instr->_sext.dst = dst;
    instr->_sext.src_type = src_type;
    instr->_sext.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_call(IR_Builder* builder, Type* proc_type, IR_OpRM proc_loc, IR_Reg dst, u32 num_args,
                               IR_InstrCallArg* args)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CALL);
    instr->_call.proc_type = proc_type;
    instr->_call.proc_loc = proc_loc;
    instr->_call.dst = dst;
    instr->_call.num_args = num_args;
    instr->_call.args = args; // TODO: Copy????

    IR_add_instr(builder, instr);
}

static void IR_patch_jmp_target(IR_Instr* jmp_instr, u32 jmp_target)
{
    switch (jmp_instr->kind)
    {
        case IR_INSTR_JMP:
            jmp_instr->_jmp.jmp_target = jmp_target;
            break;
        case IR_INSTR_JMPCC:
            jmp_instr->_jmpcc.jmp_target = jmp_target;
            break;
        default:
            assert(0);
            break;
    }
}

static u32 IR_get_jmp_target(IR_Builder* builder)
{
    return (u32)array_len(builder->curr_proc->as_proc.instrs);
}

//////////////////////////////////////////////////////
//
//      Utils for traversing AST to emit IR.
//
//////////////////////////////////////////////////////

static void IR_new_deferred_sc_jmp(IR_Builder* builder, IR_DeferredCmp* cmp, IR_ConditionKind cond, bool result,
                                   IR_Instr* instr)
{
    IR_DeferredJmpcc* new_node = NULL;

    // Pop a node off the freelist.
    if (builder->sc_jmp_freelist)
    {
        new_node = builder->sc_jmp_freelist;
        builder->sc_jmp_freelist = new_node->next;
    }
    // Create a new node.
    else
    {
        new_node = alloc_type(builder->tmp_arena, IR_DeferredJmpcc, true);
    }

    // Add node to the end of the linked-list.
    new_node->next = NULL;

    if (cmp->last_sc_jmp)
        cmp->last_sc_jmp->next = new_node;
    else
        cmp->first_sc_jmp = new_node;

    cmp->last_sc_jmp = new_node;

    // Initialize data.
    new_node->cond = cond;
    new_node->result = result;
    new_node->jmp = instr;
}

static void IR_del_deferred_sc_jmp(IR_Builder* builder, IR_DeferredCmp* cmp, IR_DeferredJmpcc* prev_jmp,
                                   IR_DeferredJmpcc* jmp)
{
    IR_DeferredJmpcc* next_jmp = jmp->next;

    // Remove short-circuit jump from list.
    if (prev_jmp)
        prev_jmp->next = next_jmp;
    else
        cmp->first_sc_jmp = next_jmp;

    // Fix last element in list.
    if (jmp == cmp->last_sc_jmp)
        cmp->last_sc_jmp = prev_jmp;

    // Add to the head of the freelist.
    jmp->next = builder->sc_jmp_freelist;
    builder->sc_jmp_freelist = jmp;
}

static void IR_mov_deferred_sc_jmp_list(IR_DeferredCmp* dst_cmp, IR_DeferredCmp* src_cmp)
{
    // Just copy list if dst is empty.
    if (!dst_cmp->first_sc_jmp)
    {
        assert(!dst_cmp->last_sc_jmp);
        dst_cmp->first_sc_jmp = src_cmp->first_sc_jmp;
        dst_cmp->last_sc_jmp = src_cmp->last_sc_jmp;
    }
    // Move non-empty source list to the end of the destination list.
    else if (src_cmp->first_sc_jmp)
    {
        assert(src_cmp->last_sc_jmp);
        dst_cmp->last_sc_jmp->next = src_cmp->first_sc_jmp;
        dst_cmp->last_sc_jmp = src_cmp->last_sc_jmp;
    }

    // Clear src list.
    src_cmp->first_sc_jmp = NULL;
    src_cmp->last_sc_jmp = NULL;
}

static void IR_copy_sc_jmp(IR_Builder* builder, IR_DeferredJmpcc* dst_jmp, IR_DeferredJmpcc* src_jmp,
                           bool desired_result)
{
    *dst_jmp = *src_jmp;

    if (dst_jmp->result != desired_result)
    {
        dst_jmp->cond = ir_opposite_cond[dst_jmp->cond];
        dst_jmp->result = desired_result;

        if (dst_jmp->jmp)
            dst_jmp->jmp->_jmpcc.cond = dst_jmp->cond;
    }

    if (!dst_jmp->jmp)
        dst_jmp->jmp = IR_emit_instr_jmpcc(builder, dst_jmp->cond, 0);
}

static void IR_execute_deferred_cmp(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEFERRED_CMP);

    IR_DeferredCmp* def_cmp = &operand->cmp;
    IR_Reg dst_reg = IR_next_reg(builder);

    bool has_sc_jmps = def_cmp->first_sc_jmp != NULL;
    bool has_final_jmp = def_cmp->final_jmp.jmp != NULL;

    if (!has_sc_jmps && !has_final_jmp)
    {
        IR_OpRM dst_arg = {.kind = IR_OP_REG, .reg = dst_reg};
        IR_emit_instr_setcc(builder, def_cmp->final_jmp.cond, dst_arg);
        IR_emit_instr_zext(builder, operand->type, dst_reg, type_u8, dst_arg);
    }
    else
    {
        IR_OpRI zero = {.kind = IR_OP_IMM, .imm.as_int._u64 = 0};
        IR_OpRI one = {.kind = IR_OP_IMM, .imm.as_int._u64 = 1};

        // Patch short-circuit jumps that jump to the "true" control path.
        for (IR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next)
        {
            if (it->result)
                IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
        }

        // This is the "true" control path. Move the literal 1 into destination register.
        IR_emit_instr_mov(builder, operand->type, dst_reg, one);

        // Patch short-circuit jumps that jump to the "false" control path.
        for (IR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next)
        {
            if (!it->result)
                IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
        }

        // Patch final jmp so that it jumps to "false" control path.
        IR_Instr* final_jmp = def_cmp->final_jmp.jmp;

        if (def_cmp->final_jmp.result)
            final_jmp->_jmpcc.cond = ir_opposite_cond[final_jmp->_jmpcc.cond];

        IR_patch_jmp_target(def_cmp->final_jmp.jmp, IR_get_jmp_target(builder));

        // This is the "false" control path. Move the literal 0 into destination register.
        IR_emit_instr_mov(builder, operand->type, dst_reg, zero);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_execute_deref(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEREF_ADDR);

    // The operand currently holds the address it is supposed to derefence.
    // This function executes the dereference into the one of the registers that held the address.
    IR_SIBDAddr addr = operand->addr;

    bool has_base = addr.base_reg < IR_REG_COUNT;
    bool has_index = addr.scale && (addr.index_reg < IR_REG_COUNT);

    assert(has_base || has_index);

    IR_Reg dst_reg = has_base ? addr.base_reg : addr.index_reg;

    IR_emit_instr_load_ptr(builder, operand->type, dst_reg, addr);

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;

    if (has_base && (addr.base_reg != dst_reg))
        IR_free_reg(builder, addr.base_reg);
    if (has_index && (addr.index_reg != dst_reg))
        IR_free_reg(builder, addr.index_reg);
}

static void IR_execute_lea(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_SIBD_ADDR);

    // The operand currently holds a memory address.
    // This function executes the "load-effective-address" call into the one of the registers that held the address.
    IR_SIBDAddr addr = operand->addr;

    bool has_base = addr.base_reg < IR_REG_COUNT;
    bool has_index = addr.scale && (addr.index_reg < IR_REG_COUNT);
    bool has_disp = addr.disp != 0;
    IR_Reg dst_reg;

    assert(has_base || has_index);

    if (has_base && !has_index && !has_disp)
    {
        // No need to emit any instructions. Just keep address in base register.
        dst_reg = operand->addr.base_reg;
    }
    else
    {
        dst_reg = has_base ? addr.base_reg : addr.index_reg;

        IR_emit_instr_laddr_ptr(builder, dst_reg, addr);

        if (has_base && (addr.base_reg != dst_reg))
            IR_free_reg(builder, addr.base_reg);
        if (has_index && (addr.index_reg != dst_reg))
            IR_free_reg(builder, addr.index_reg);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_commit_indirections(IR_Builder* builder, IR_Operand* operand)
{
    if (operand->kind == IR_OPERAND_DEREF_ADDR)
        IR_execute_deref(builder, operand);
    else if (operand->kind == IR_OPERAND_SIBD_ADDR)
        IR_execute_lea(builder, operand);
    else if (operand->kind == IR_OPERAND_DEFERRED_CMP)
        IR_execute_deferred_cmp(builder, operand);
}

static void IR_ensure_operand_in_reg(IR_Builder* builder, IR_Operand* operand, bool commit_ptr)
{
    if (commit_ptr && (operand->kind == IR_OPERAND_SIBD_ADDR))
    {
        IR_execute_lea(builder, operand);
    }
    else if (operand->kind == IR_OPERAND_DEREF_ADDR)
    {
        IR_execute_deref(builder, operand);
    }
    else if (operand->kind == IR_OPERAND_DEFERRED_CMP)
    {
        IR_execute_deferred_cmp(builder, operand);
    }
    else if (operand->kind != IR_OPERAND_REG)
    {
        if (!commit_ptr && (operand->type->kind == TYPE_PTR))
        {
            if (operand->kind != IR_OPERAND_SIBD_ADDR)
            {
                IR_Reg base_reg = IR_next_reg(builder);
                IR_emit_instr_load_var(builder, operand->type, base_reg, operand->sym);

                operand->kind = IR_OPERAND_SIBD_ADDR;
                operand->addr.base_reg = base_reg;
                operand->addr.index_reg = IR_REG_COUNT;
                operand->addr.scale = 0;
                operand->addr.disp = 0;
            }
        }
        else if (operand->kind == IR_OPERAND_IMM)
        {
            IR_Reg reg = IR_next_reg(builder);
            IR_OpRI imm_arg = {.kind = IR_OP_IMM, .imm = operand->imm};

            IR_emit_instr_mov(builder, operand->type, reg, imm_arg);

            operand->kind = IR_OPERAND_REG;
            operand->reg = reg;
        }
        else
        {
            IR_Reg reg = IR_next_reg(builder);
            IR_emit_instr_load_var(builder, operand->type, reg, operand->sym);

            operand->kind = IR_OPERAND_REG;
            operand->reg = reg;
        }
    }
}

static IR_OpRM IR_oprm_from_op(IR_Builder* builder, IR_Operand* op)
{
    IR_OpRM arg = {0};

    switch (op->kind)
    {
        case IR_OPERAND_IMM:
            IR_ensure_operand_in_reg(builder, op, true);
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        case IR_OPERAND_REG:
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        case IR_OPERAND_VAR:
            arg.kind = IR_OP_MEM;
            arg.mem.kind = IR_MEM_ADDR_SYM;
            arg.mem.sym = op->sym;
            break;
        case IR_OPERAND_DEREF_ADDR:
            IR_execute_deref(builder, op);
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        case IR_OPERAND_DEFERRED_CMP:
            IR_execute_deferred_cmp(builder, op);
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        case IR_OPERAND_SIBD_ADDR:
            IR_execute_lea(builder, op);
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        default:
            assert(!0);
            break;
    }

    return arg;
}
static IR_OpRMI IR_oprmi_from_op(IR_Builder* builder, IR_Operand* op)
{
    IR_OpRMI arg = {0};

    switch (op->kind)
    {
        case IR_OPERAND_IMM:
            arg.kind = IR_OP_IMM;
            arg.imm = op->imm;
            break;
        case IR_OPERAND_REG:
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        case IR_OPERAND_VAR:
            arg.kind = IR_OP_MEM;
            arg.mem.kind = IR_MEM_ADDR_SYM;
            arg.mem.sym = op->sym;
            break;
        case IR_OPERAND_DEREF_ADDR:
            IR_execute_deref(builder, op);
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        case IR_OPERAND_DEFERRED_CMP:
            IR_execute_deferred_cmp(builder, op);
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        case IR_OPERAND_SIBD_ADDR:
            IR_execute_lea(builder, op);
            arg.kind = IR_OP_REG;
            arg.reg = op->reg;
            break;
        default:
            assert(!0);
            break;
    }

    return arg;
}

//////////////////////////////////////////////////////
//
//      Traverse AST to emit IR.
//
//////////////////////////////////////////////////////
static void IR_operand_from_sym(IR_Operand* op, Symbol* sym)
{
    if (sym->kind == SYMBOL_VAR)
    {
        op->kind = IR_OPERAND_VAR;
        op->type = sym->type;
        op->sym = sym;
    }
    else if (sym->kind == SYMBOL_PROC)
    {
        op->kind = IR_OPERAND_PROC;
        op->type = sym->type;
        op->sym = sym;
    }
    else
    {
        assert(0);
    }
}

static void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst);

static void IR_emit_expr_ident(IR_Builder* builder, ExprIdent* eident, IR_Operand* dst)
{
    Symbol* sym = lookup_symbol(builder->curr_scope, eident->name);
    IR_operand_from_sym(dst, sym);
}

static void IR_emit_ptr_int_add(IR_Builder* builder, IR_Operand* dst, IR_Operand* ptr_op, IR_Operand* int_op, bool add)
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
            IR_OpRM dst_arg = {.kind = IR_OP_REG, .reg = ptr_op->addr.index_reg};
            IR_OpRMI src_arg = IR_oprmi_from_op(builder, int_op);

            if (add)
                IR_emit_instr_add(builder, type_s64, dst_arg, src_arg);
            else
                IR_emit_instr_sub(builder, type_s64, dst_arg, src_arg);

            IR_try_free_op_reg(builder, int_op);
        }
        else
        {
            IR_ensure_operand_in_reg(builder, int_op, true);

            if (!add)
            {
                IR_OpRM arg = {.kind = IR_OP_REG, .reg = int_op->reg};
                IR_emit_instr_neg(builder, int_op->type, arg);
            }

            ptr_op->addr.scale = base_size;
            ptr_op->addr.index_reg = int_op->reg;
        }
    }

    *dst = *ptr_op;
}

static void IR_emit_binary_cmp(IR_Builder* builder, IR_ConditionKind cond_kind, Type* dst_type, IR_Operand* dst_op,
                               IR_Operand* left_op, IR_Operand* right_op)
{
    assert(left_op->type == right_op->type);

    IR_commit_indirections(builder, left_op);

    IR_OpRM left_arg;
    IR_OpRMI right_arg = IR_oprmi_from_op(builder, right_op);

    if (right_arg.kind != IR_OP_MEM)
    {
        left_arg = IR_oprm_from_op(builder, left_op);
    }
    else
    {
        IR_ensure_operand_in_reg(builder, left_op, true);
        left_arg = (IR_OpRM){.kind = IR_OP_REG, .reg = left_op->reg};
    }

    IR_emit_instr_cmp(builder, left_op->type, left_arg, right_arg);

    dst_op->type = dst_type;
    dst_op->kind = IR_OPERAND_DEFERRED_CMP;
    dst_op->cmp.final_jmp.cond = cond_kind;
    dst_op->cmp.final_jmp.result = true;
    dst_op->cmp.final_jmp.jmp = NULL;
    dst_op->cmp.first_sc_jmp = NULL;
    dst_op->cmp.last_sc_jmp = NULL;

    IR_try_free_op_reg(builder, left_op);
    IR_try_free_op_reg(builder, right_op);
}

static void IR_emit_short_circuit_cmp(IR_Builder* builder, IR_Operand* dst_op, ExprBinary* expr)
{
    IR_OpRMI zero_arg = {.kind = IR_OP_IMM, .imm.as_int._u64 = 0};

    //
    // NOTE: This procedure will create a deferred comparison containing an array of short-circuit jumps and one final
    // jump. If the left and right subexpressions are themselves deferred comparisons, then they will be merged into
    // this parent expression's deferred comparison. Otherwise, subexpressions that are not deferred comparisons will be
    // compared to zero and converted to either a short-circuit jump (left subexpression) or a final jump (right
    // subexpression).
    //

    dst_op->kind = IR_OPERAND_DEFERRED_CMP;
    dst_op->type = expr->super.type;

    IR_Operand left_op = {0};
    IR_Operand right_op = {0};

    bool short_circuit_val;
    IR_ConditionKind short_circuit_cond;

    if (expr->op == TKN_LOGIC_AND)
    {
        short_circuit_val = false;
        short_circuit_cond = IR_COND_EQ;
    }
    else
    {
        assert(expr->op == TKN_LOGIC_OR);
        short_circuit_val = true;
        short_circuit_cond = IR_COND_NEQ;
    }

    // Emit instructions for the left expression.
    IR_emit_expr(builder, expr->left, &left_op);

    // If the left subexpression is a deferred comparison, merge into this deferred comparison result.
    //
    // Short-circuit jumps from the left subexpression with the same "short-circuit value" are kept as-is.
    //
    // Short-circuit jumps from the left subexpression with the opposite "short-circuit value" are patched
    // with the current instruction index as the jump target and removed. This ensures that short-circuit jumps
    // with the opposite "short-circuit value" are compared to the right subexpression.
    //
    // The left subexpression's final jump is added as a short-circuit jump.
    if (left_op.kind == IR_OPERAND_DEFERRED_CMP)
    {
        // Copy list of short-circuit jumps.
        dst_op->cmp.first_sc_jmp = left_op.cmp.first_sc_jmp;
        dst_op->cmp.last_sc_jmp = left_op.cmp.last_sc_jmp;

        // Patch and remove short-circuit jumps with the opposite "short-circuit value".
        IR_DeferredJmpcc* it = dst_op->cmp.first_sc_jmp;
        IR_DeferredJmpcc* prev_it = NULL;

        while (it)
        {
            IR_DeferredJmpcc* next_it = it->next;

            if (it->result != short_circuit_val)
            {
                IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
                IR_del_deferred_sc_jmp(builder, &dst_op->cmp, prev_it, it);
            }

            it = next_it;
            prev_it = it;
        }

        // Convert left expression's final jmp to a short-circuit jmp.
        IR_DeferredJmpcc j;
        IR_copy_sc_jmp(builder, &j, &left_op.cmp.final_jmp, short_circuit_val);
        IR_new_deferred_sc_jmp(builder, &dst_op->cmp, j.cond, j.result, j.jmp);
    }

    // The left subexpression is some computation (not a deferred comparison). Compare the left subexpression to zero
    // and create a short-circuit jmp.
    else
    {
        IR_commit_indirections(builder, &left_op);

        IR_OpRM left_arg = IR_oprm_from_op(builder, &left_op);
        IR_emit_instr_cmp(builder, left_op.type, left_arg, zero_arg);
        IR_Instr* jmpcc_instr = IR_emit_instr_jmpcc(builder, short_circuit_cond, 0);

        IR_new_deferred_sc_jmp(builder, &dst_op->cmp, short_circuit_cond, short_circuit_val, jmpcc_instr);
        IR_try_free_op_reg(builder, &left_op);
    }

    // Emit instructions for the right expression.
    IR_emit_expr(builder, expr->right, &right_op);

    // If the right subexpression is a deferred comparison, merge into this deferred comparison result.
    // The right subexpression's short-circuit jumps are kept as-is.
    // The right subexpression's final jump is converted to a final jump to the "false" control path.
    if (right_op.kind == IR_OPERAND_DEFERRED_CMP)
    {
        // Merge lists of short-circuit jumps.
        IR_mov_deferred_sc_jmp_list(&dst_op->cmp, &right_op.cmp);

        // Convert the right expression's final jmp into a final jmp to the "false" path.
        IR_copy_sc_jmp(builder, &dst_op->cmp.final_jmp, &right_op.cmp.final_jmp, false);
    }
    // The right subexpression is some computation (not a deferred comparison). Compare the right subexpression to zero
    // and create a final jump.
    else
    {
        IR_commit_indirections(builder, &right_op);

        IR_OpRM right_arg = IR_oprm_from_op(builder, &right_op);
        IR_emit_instr_cmp(builder, right_op.type, right_arg, zero_arg);

        dst_op->cmp.final_jmp.result = false;
        dst_op->cmp.final_jmp.jmp = IR_emit_instr_jmpcc(builder, IR_COND_EQ, 0);
        dst_op->cmp.final_jmp.cond = IR_COND_EQ;

        IR_try_free_op_reg(builder, &right_op);
    }
}

static void IR_emit_expr_binary(IR_Builder* builder, ExprBinary* expr, IR_Operand* dst)
{
    Type* result_type = expr->super.type;
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
                assert(result_type == left.type);
                IR_emit_ptr_int_add(builder, dst, &left, &right, true);
            }
            else if (right_is_ptr)
            {
                assert(result_type == right.type);
                IR_emit_ptr_int_add(builder, dst, &right, &left, true);
            }
            else
            {
                assert(left.type == right.type);
                assert(result_type == left.type);
                assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

                if (left.kind == IR_OPERAND_IMM)
                {
                    IR_ensure_operand_in_reg(builder, &right, true);

                    IR_OpRM dst_arg = {.kind = IR_OP_REG, .reg = right.reg};
                    IR_OpRMI src_arg = {.kind = IR_OP_IMM, .imm = left.imm};
                    IR_emit_instr_add(builder, result_type, dst_arg, src_arg);

                    *dst = right;
                    IR_try_free_op_reg(builder, &left);
                }
                else
                {
                    IR_ensure_operand_in_reg(builder, &left, true);

                    IR_OpRM dst_arg = {.kind = IR_OP_REG, .reg = left.reg};
                    IR_OpRMI src_arg = IR_oprmi_from_op(builder, &right);
                    IR_emit_instr_add(builder, result_type, dst_arg, src_arg);

                    *dst = left;
                    IR_try_free_op_reg(builder, &right);
                }
            }
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
                assert(result_type == left.type);
                IR_emit_ptr_int_add(builder, dst, &left, &right, false);
            }
            // ptr - ptr => s64
            else if (left_is_ptr && right_is_ptr)
            {
                assert(result_type == left.type);

                u64 base_size = left.type->as_ptr.base->size;
                u32 base_size_log2 = (u32)clp2(base_size);

                IR_ensure_operand_in_reg(builder, &left, true);

                IR_OpRM dst_arg = {.kind = IR_OP_REG, .reg = left.reg};
                IR_OpRMI src_arg = IR_oprmi_from_op(builder, &right);
                IR_emit_instr_sub(builder, result_type, dst_arg, src_arg);

                if (base_size_log2)
                {
                    IR_OpRMI shift_arg = {.kind = IR_OP_IMM, .imm.as_int._u32 = base_size_log2};
                    IR_emit_instr_shr(builder, result_type, dst_arg, shift_arg);
                }

                *dst = left;
                IR_try_free_op_reg(builder, &right);
            }
            // int - int => int
            else
            {
                assert(left.type == right.type);
                assert(result_type == left.type);
                assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

                IR_ensure_operand_in_reg(builder, &left, true);

                IR_OpRM dst_arg = {.kind = IR_OP_REG, .reg = left.reg};
                IR_OpRMI src_arg = IR_oprmi_from_op(builder, &right);
                IR_emit_instr_sub(builder, result_type, dst_arg, src_arg);

                *dst = left;
                IR_try_free_op_reg(builder, &right);
            }
            break;
        }
        case TKN_EQ:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);
            IR_emit_binary_cmp(builder, IR_COND_EQ, result_type, dst, &left, &right);
            break;
        }
        case TKN_NOTEQ:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);
            IR_emit_binary_cmp(builder, IR_COND_NEQ, result_type, dst, &left, &right);
            break;
        }
        case TKN_LT:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_LT : IR_COND_U_LT;

            IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
            break;
        }
        case TKN_LTEQ:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_LTEQ : IR_COND_U_LTEQ;

            IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
            break;
        }
        case TKN_GT:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_GT : IR_COND_U_GT;

            IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
            break;
        }
        case TKN_GTEQ:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_GTEQ : IR_COND_U_GTEQ;

            IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
            break;
        }
        case TKN_LOGIC_AND:
        case TKN_LOGIC_OR:
        {
            IR_emit_short_circuit_cmp(builder, dst, expr);
            break;
        }
        default:
            assert(0);
            break;
    }
}

static void IR_emit_expr_unary(IR_Builder* builder, ExprUnary* expr, IR_Operand* dst)
{
    Type* result_type = expr->super.type;

    switch (expr->op)
    {
        case TKN_PLUS:
        {
            IR_emit_expr(builder, expr->expr, dst);
            break;
        }
        case TKN_MINUS: // Two's compliment negation.
        {
            IR_emit_expr(builder, expr->expr, dst);
            IR_ensure_operand_in_reg(builder, dst, true);

            assert(dst->type == result_type);

            IR_OpRM arg = {.kind = IR_OP_REG, .reg = dst->reg};
            IR_emit_instr_neg(builder, result_type, arg);
            break;
        }
        case TKN_NEG: // Bitwise not
        {

            IR_emit_expr(builder, expr->expr, dst);
            IR_ensure_operand_in_reg(builder, dst, true);

            assert(dst->type == result_type);

            IR_OpRM arg = {.kind = IR_OP_REG, .reg = dst->reg};
            IR_emit_instr_not(builder, result_type, arg);
            break;
        }
        case TKN_NOT: // Logical not
        {
            dst->kind = IR_OPERAND_DEFERRED_CMP;
            dst->type = result_type;

            IR_Operand inner_op = {0};
            IR_emit_expr(builder, expr->expr, &inner_op);

            if (inner_op.kind == IR_OPERAND_DEFERRED_CMP)
            {
                // Reverse control paths for all jumps.
                // Ex: if a jmp instruction jumps to the "true" path, make it jump to the "false" path.
                dst->cmp.first_sc_jmp = inner_op.cmp.first_sc_jmp;
                dst->cmp.last_sc_jmp = inner_op.cmp.last_sc_jmp;

                for (IR_DeferredJmpcc* it = dst->cmp.first_sc_jmp; it; it = it->next)
                {
                    it->result = !(it->result);
                }

                dst->cmp.final_jmp = inner_op.cmp.final_jmp;
                dst->cmp.final_jmp.result = !inner_op.cmp.final_jmp.result;
            }
            else
            {
                IR_commit_indirections(builder, &inner_op);

                assert(inner_op.type == result_type);

                IR_OpRM dst_arg = IR_oprm_from_op(builder, &inner_op);
                IR_OpRMI zero_arg = {.kind = IR_OP_IMM, .imm.as_int._u64 = 0};

                IR_emit_instr_cmp(builder, result_type, dst_arg, zero_arg);

                dst->cmp.final_jmp.cond = IR_COND_EQ;
                dst->cmp.final_jmp.result = true;
                dst->cmp.first_sc_jmp = NULL;
                dst->cmp.last_sc_jmp = NULL;
                dst->cmp.final_jmp.jmp = NULL;

                IR_try_free_op_reg(builder, &inner_op);
            }

            break;
        }
        case TKN_ASTERISK:
        {
            IR_emit_expr(builder, expr->expr, dst);
            IR_ensure_operand_in_reg(builder, dst, false);

            dst->kind = IR_OPERAND_DEREF_ADDR;
            dst->type = result_type;
            break;
        }
        case TKN_CARET: // Address-of operator
        {
            IR_emit_expr(builder, expr->expr, dst);

            if (dst->kind == IR_OPERAND_DEREF_ADDR)
            {
                dst->kind = IR_OPERAND_SIBD_ADDR;
                dst->type = result_type;
            }
            else
            {
                assert(dst->kind == IR_OPERAND_VAR);

                IR_Reg result_reg = IR_next_reg(builder);

                IR_emit_instr_laddr_var(builder, result_reg, dst->sym);

                dst->kind = IR_OPERAND_SIBD_ADDR;
                dst->type = result_type;
                dst->addr.base_reg = result_reg;
                dst->addr.index_reg = IR_REG_COUNT;
                dst->addr.disp = 0;
                dst->addr.scale = 0;
            }
            break;
        }
        default:
            assert(0);
            break;
    }
}

static void IR_emit_expr_index(IR_Builder* builder, ExprIndex* expr_index, IR_Operand* dst)
{
    IR_Operand array_op = {0};
    IR_Operand index_op = {0};

    IR_emit_expr(builder, expr_index->array, &array_op);
    IR_emit_expr(builder, expr_index->index, &index_op);

    assert(array_op.type->kind == TYPE_PTR);

    IR_emit_ptr_int_add(builder, dst, &array_op, &index_op, true);

    dst->kind = IR_OPERAND_DEREF_ADDR;
    dst->type = expr_index->super.type;
}

static void IR_emit_int_cast(IR_Builder* builder, IR_Operand* src_op, IR_Operand* dst_op)
{
    // NOTE:
    // This function treats pointers like integers. The IR currently implements "opaque" pointers, so
    // there are no explicit instructions for converting from one ptr type to another, or converting to/from int/ptr.
    assert(src_op->kind != IR_OPERAND_IMM); // Should be prevented by resolver.

    // The src expression could be a deferred dereference or a deferred SIBD address.
    // We need the src expression to be a concrete value.
    IR_commit_indirections(builder, src_op);

    IR_Reg dst_reg = IR_REG_COUNT;

    if (src_op->type->size == dst_op->type->size)
    {
        // This is a NO-OP even if any of the types is a ptr type.
        // Just make sure the result is in a register.
        if (src_op->kind == IR_OPERAND_REG)
        {
            dst_reg = src_op->reg;
        }
        else
        {
            assert(src_op->kind == IR_OPERAND_VAR);

            dst_reg = IR_next_reg(builder);
            IR_emit_instr_load_var(builder, src_op->type, dst_reg, src_op->sym);
        }
    }
    // Truncate from larger type to smaller type.
    else if (src_op->type->size > dst_op->type->size)
    {
        dst_reg = IR_next_reg(builder);

        if (src_op->kind == IR_OPERAND_REG)
        {
            IR_OpRM src_arg = {.kind = IR_OP_REG, .reg = src_op->reg};
            IR_emit_instr_trunc(builder, dst_op->type, dst_reg, src_op->type, src_arg);

            IR_try_free_op_reg(builder, src_op);
        }
        else
        {
            assert(src_op->kind == IR_OPERAND_VAR);

            IR_OpRM src_arg = {.kind = IR_OP_MEM, .mem = {.kind = IR_MEM_ADDR_SYM, .sym = src_op->sym}};
            IR_emit_instr_trunc(builder, dst_op->type, dst_reg, src_op->type, src_arg);
        }
    }
    // Extend (sign or zero) to larger type.
    else
    {
        assert(src_op->type->size < dst_op->type->size);

        dst_reg = IR_next_reg(builder);
        bool src_signed = (src_op->type->kind == TYPE_INTEGER) && src_op->type->as_integer.is_signed;

        if (src_op->kind == IR_OPERAND_REG)
        {
            IR_OpRM src_arg = {.kind = IR_OP_REG, .reg = src_op->reg};

            if (src_signed)
                IR_emit_instr_sext(builder, dst_op->type, dst_reg, src_op->type, src_arg);
            else
                IR_emit_instr_zext(builder, dst_op->type, dst_reg, src_op->type, src_arg);

            IR_try_free_op_reg(builder, src_op);
        }
        else
        {
            IR_OpRM src_arg = {.kind = IR_OP_MEM, .mem = {.kind = IR_MEM_ADDR_SYM, .sym = src_op->sym}};

            if (src_signed)
                IR_emit_instr_sext(builder, dst_op->type, dst_reg, src_op->type, src_arg);
            else
                IR_emit_instr_zext(builder, dst_op->type, dst_reg, src_op->type, src_arg);
        }
    }

    if (dst_op->type->kind == TYPE_PTR)
    {
        dst_op->kind = IR_OPERAND_SIBD_ADDR;
        dst_op->addr.base_reg = dst_reg;
        dst_op->addr.index_reg = IR_REG_COUNT;
        dst_op->addr.scale = 0;
        dst_op->addr.disp = 0;
    }
    else
    {
        dst_op->kind = IR_OPERAND_REG;
        dst_op->reg = dst_reg;
    }
}

static void IR_emit_expr_cast(IR_Builder* builder, ExprCast* expr_cast, IR_Operand* dst_op)
{
    // Emit instructions for source expression that will be casted.
    IR_Operand src_op = {0};
    IR_emit_expr(builder, expr_cast->expr, &src_op);

    // Fill in the type for the overall cast expression.
    dst_op->type = expr_cast->super.type;

    // TODO: Support floats.
    assert(src_op.type->kind != TYPE_FLOAT);
    assert(dst_op->type->kind != TYPE_FLOAT);
    assert(src_op.type != dst_op->type); // Should be prevented by resolver.

    if (src_op.type->kind == TYPE_ARRAY && dst_op->type->kind == TYPE_PTR)
    {
        assert(src_op.kind == IR_OPERAND_VAR);
        IR_Reg dst_reg = IR_next_reg(builder);
        IR_emit_instr_laddr_var(builder, dst_reg, src_op.sym);

        dst_op->kind = IR_OPERAND_SIBD_ADDR;
        dst_op->addr.base_reg = dst_reg;
        dst_op->addr.index_reg = IR_REG_COUNT;
        dst_op->addr.disp = 0;
        dst_op->addr.scale = 0;
    }
    else
    {
        IR_emit_int_cast(builder, &src_op, dst_op);
    }
}

static bool IR_type_fits_in_reg(Type* type)
{
    return type->size <= PTR_SIZE;
}

static void IR_emit_expr_call(IR_Builder* builder, ExprCall* expr_call, IR_Operand* dst_op)
{
    u32 num_args = (u32)expr_call->num_args;
    IR_InstrCallArg* args = alloc_array(builder->arena, IR_InstrCallArg, num_args, false);

    // Emit instructions for each argument expression and collect the resulting expression values
    // into an `args` array.
    u32 arg_index = 0;
    List* head = &expr_call->args;
    List* it = head->next;

    while (it != head)
    {
        ProcCallArg* ast_arg = list_entry(it, ProcCallArg, lnode);
        IR_Operand arg_op = {0};

        IR_emit_expr(builder, ast_arg->expr, &arg_op);

        if (IR_type_fits_in_reg(arg_op.type))
        {
            IR_ensure_operand_in_reg(builder, &arg_op, true);

            args[arg_index].type = arg_op.type;
            args[arg_index].loc.kind = IR_OP_REG;
            args[arg_index].loc.reg = arg_op.reg;
        }
        else
        {
            // TODO: Support struct types
            assert(0);
        }

        IR_try_free_op_reg(builder, &arg_op);

        arg_index += 1;
        it = it->next;
    }

    // Emit instructions for the procedure pointer/name.
    IR_Operand proc_op = {0};
    IR_OpRM proc_loc = {0};

    IR_emit_expr(builder, expr_call->proc, &proc_op);

    if (proc_op.kind == IR_OPERAND_PROC)
    {
        proc_loc.kind = IR_OP_MEM;
        proc_loc.mem.kind = IR_MEM_ADDR_SYM;
        proc_loc.mem.sym = proc_op.sym;
    }
    else
    {
        IR_ensure_operand_in_reg(builder, &proc_op, true);
        proc_loc.kind = IR_OP_REG;
        proc_loc.reg = proc_op.reg;
    }

    // Allocate a return value register if procedure returns a value.
    Type* ret_type = expr_call->super.type;

    if (ret_type != type_void)
    {
        if (IR_type_fits_in_reg(ret_type))
        {
            dst_op->kind = IR_OPERAND_REG;
            dst_op->type = ret_type;
            dst_op->reg = IR_next_reg(builder);
        }
        else
        {
            // TODO: Support returning structs
            assert(0);
        }
    }
    else
    {
        dst_op->kind = IR_OPERAND_NONE;
        dst_op->type = type_void;
        dst_op->reg = IR_REG_COUNT;
    }

    // Emit call instruction.
    IR_emit_instr_call(builder, proc_op.type, proc_loc, dst_op->reg, num_args, args);

    // Clean up
    IR_try_free_op_reg(builder, &proc_op);
}

static void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst)
{
    if (expr->is_const)
    {
        dst->kind = IR_OPERAND_IMM;
        dst->type = expr->type;
        dst->imm = expr->const_val;

        return;
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

// Forward declare
static void IR_emit_stmt(IR_Builder* builder, Stmt* stmt);

static void IR_push_scope(IR_Builder* builder, Scope* scope)
{
    builder->curr_scope = scope;
}

static void IR_pop_scope(IR_Builder* builder)
{
    builder->curr_scope = builder->curr_scope->parent;
}

static void IR_emit_assign(IR_Builder* builder, IR_Operand* lhs, IR_Operand* rhs)
{
    switch (lhs->kind)
    {
        case IR_OPERAND_VAR:
        {
            if (rhs->kind == IR_OPERAND_IMM)
            {
                IR_OpRI rhs_arg = {.kind = IR_OP_IMM, .imm = rhs->imm};
                IR_emit_instr_store_var(builder, lhs->type, lhs->sym, rhs_arg);
            }
            else
            {
                IR_ensure_operand_in_reg(builder, rhs, true);

                IR_OpRI rhs_arg = {.kind = IR_OP_REG, .reg = rhs->reg};
                IR_emit_instr_store_var(builder, lhs->type, lhs->sym, rhs_arg);
            }

            break;
        }
        case IR_OPERAND_DEREF_ADDR:
        {
            if (rhs->kind == IR_OPERAND_IMM)
            {
                IR_OpRI rhs_arg = {.kind = IR_OP_IMM, .imm = rhs->imm};
                IR_emit_instr_store_ptr(builder, lhs->type, lhs->addr, rhs_arg);
            }
            else
            {
                IR_ensure_operand_in_reg(builder, rhs, true);

                IR_OpRI rhs_arg = {.kind = IR_OP_REG, .reg = rhs->reg};
                IR_emit_instr_store_ptr(builder, lhs->type, lhs->addr, rhs_arg);
            }

            break;
        }
        default:
            assert(0);
            break;
    }
}

static void IR_emit_stmt_block_body(IR_Builder* builder, List* stmts)
{
    List* head = stmts;
    List* it = head->next;

    while (it != head)
    {
        Stmt* s = list_entry(it, Stmt, lnode);

        IR_emit_stmt(builder, s);

        it = it->next;
    }
}

static void IR_emit_stmt_block(IR_Builder* builder, StmtBlock* sblock)
{
    IR_push_scope(builder, sblock->scope);
    IR_emit_stmt_block_body(builder, &sblock->stmts);
    IR_pop_scope(builder);
}

static void IR_emit_stmt_return(IR_Builder* builder, StmtReturn* sret)
{
    IR_Operand expr_op = {0};
    IR_emit_expr(builder, sret->expr, &expr_op);
    IR_ensure_operand_in_reg(builder, &expr_op, true);

    IR_emit_instr_ret(builder, expr_op.type, expr_op.reg);

    IR_try_free_op_reg(builder, &expr_op);
}

static void IR_emit_stmt_expr(IR_Builder* builder, StmtExpr* sexpr)
{
    IR_Operand expr_op = {0};
    IR_emit_expr(builder, sexpr->expr, &expr_op);
    IR_commit_indirections(builder, &expr_op);
    IR_try_free_op_reg(builder, &expr_op);
}

static void IR_emit_stmt_expr_assign(IR_Builder* builder, StmtExprAssign* stmt)
{
    switch (stmt->op_assign)
    {
        case TKN_ASSIGN:
        {
            IR_Operand lhs_op = {0};
            IR_Operand rhs_op = {0};

            IR_emit_expr(builder, stmt->left, &lhs_op);
            IR_emit_expr(builder, stmt->right, &rhs_op);

            IR_emit_assign(builder, &lhs_op, &rhs_op);

            IR_try_free_op_reg(builder, &lhs_op);
            IR_try_free_op_reg(builder, &rhs_op);
            break;
        }
        default:
            assert(!"Unsupported assignment operator in IR generation");
            break;
    }
}

static void IR_emit_stmt_decl(IR_Builder* builder, StmtDecl* sdecl)
{
    assert(sdecl->decl->kind == CST_DeclVar);

    DeclVar* dvar = (DeclVar*)sdecl->decl;

    if (dvar->init)
    {
        IR_Operand rhs_op;
        IR_Operand lhs_op;

        IR_emit_expr(builder, dvar->init, &rhs_op);
        IR_operand_from_sym(&lhs_op, lookup_symbol(builder->curr_scope, dvar->name));

        IR_emit_assign(builder, &lhs_op, &rhs_op);

        IR_try_free_op_reg(builder, &lhs_op);
        IR_try_free_op_reg(builder, &rhs_op);
    }
}

static void IR_emit_stmt_if(IR_Builder* builder, StmtIf* stmt)
{
    Expr* cond_expr = stmt->if_blk.cond;
    Stmt* if_body = stmt->if_blk.body;
    Stmt* else_body = stmt->else_blk.body;

    // If expr is a compile-time constant, do not generate the unneeded branch!!
    if (cond_expr->is_const)
    {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        if (cond_val)
            IR_emit_stmt(builder, if_body);
        else
            IR_emit_stmt(builder, else_body);
    }
    else
    {
        IR_Operand cond_op = {0};
        IR_emit_expr(builder, cond_expr, &cond_op);

        IR_Instr* jmpcc_false = NULL;

        // If the condition is a chain of deferred comparisons, first patch the jump targets
        // for all short-circuit jumps that jump to the "true" path, which corresponds to the
        // current instruction index.
        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP)
        {
            // Patch the jump target for all short-circuit jmps that jump to the "true" control path.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next)
            {
                if (it->result)
                    IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
            }

            // Create a jump to the "false" path if it doesn't yet exist.
            if (!cond_op.cmp.final_jmp.jmp)
                cond_op.cmp.final_jmp.jmp = IR_emit_instr_jmpcc(builder, cond_op.cmp.final_jmp.cond, 0);
        }
        // If the condition is some computation, compare the condition to zero and create a conditional
        // jump to the "false" path.
        else
        {
            IR_ensure_operand_in_reg(builder, &cond_op, true);

            IR_OpRM cond_arg = {.kind = IR_OP_REG, .reg = cond_op.reg};
            IR_OpRMI zero_arg = {.kind = IR_OP_IMM, .imm.as_int._u64 = 0};

            // TODO: Add a true "boolean" type. If the condition expression is already a "boolean" type,
            // then this cmp instruction is NOT necessary!!
            IR_emit_instr_cmp(builder, cond_op.type, cond_arg, zero_arg);

            // Emit conditional jump without a jump target. The jump target will be filled in below.
            jmpcc_false = IR_emit_instr_jmpcc(builder, IR_COND_EQ, 0);

            IR_try_free_op_reg(builder, &cond_op);
        }

        // Emit instructions for if-block body.
        IR_emit_stmt(builder, if_body);

        if (else_body)
        {
            // Code path from if-block needs to jump over the else block. However, this is not necessary if 
            // all code paths within the if-block return.
            IR_Instr* jmp_end_instr = if_body->returns ? NULL : IR_emit_instr_jmp(builder, 0);

            // Patch conditional jmp instruction(s) to jump to the else-block when the condition is false.
            if (cond_op.kind == IR_OPERAND_DEFERRED_CMP)
            {
                // Patch the jump target for all short-circuit jmps that jump to the "false" control path.
                for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next)
                {
                    if (!it->result)
                        IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
                }

                // Patch final jmp to the "false" control path.
                if (cond_op.cmp.final_jmp.result)
                    cond_op.cmp.final_jmp.jmp->_jmpcc.cond = ir_opposite_cond[cond_op.cmp.final_jmp.cond];

                IR_patch_jmp_target(cond_op.cmp.final_jmp.jmp, IR_get_jmp_target(builder));
            }
            else
            {
                IR_patch_jmp_target(jmpcc_false, IR_get_jmp_target(builder));
            }

            // Emit instructions for else-block body.
            IR_emit_stmt(builder, else_body);

            // Patch jmp instruction that jumps to the end of the else-block.
            if (jmp_end_instr)
                IR_patch_jmp_target(jmp_end_instr, IR_get_jmp_target(builder));
        }
        else
        {
            // Patch conditional jmp instruction(s) to jump after the if-block when the condition is false.
            if (cond_op.kind == IR_OPERAND_DEFERRED_CMP)
            {
                // Patch the jump target for all short-circuit jmps that jump to the "false" control path.
                for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next)
                {
                    if (!it->result)
                        IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
                }

                // Patch final jmp to "false" control path.
                if (cond_op.cmp.final_jmp.result)
                    cond_op.cmp.final_jmp.jmp->_jmpcc.cond = ir_opposite_cond[cond_op.cmp.final_jmp.cond];

                IR_patch_jmp_target(cond_op.cmp.final_jmp.jmp, IR_get_jmp_target(builder));
            }
            else
            {
                IR_patch_jmp_target(jmpcc_false, IR_get_jmp_target(builder));
            }
        }
    }
}

static void IR_emit_stmt_while(IR_Builder* builder, StmtWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_const)
    {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val)
        {
            u32 loop_top = IR_get_jmp_target(builder);

            // Emit loop body statements.
            IR_emit_stmt(builder, body);

            // Jump back to the top of the loop.
            IR_emit_instr_jmp(builder, loop_top);
        }
    }
    else
    {
        // Emit jmp instruction to the loop's condition check. Target adddress
        // will be patched.
        IR_Instr* jmp_instr = IR_emit_instr_jmp(builder, 0);

        // Save the current instruction index to enable jumps to the top of the loop.
        u32 loop_top = IR_get_jmp_target(builder);

        // Emit instructions for the loop body.
        IR_emit_stmt(builder, body);

        // Patch initial jmp instruction with the location of the condition check.
        u32 loop_cond_check = IR_get_jmp_target(builder);
        IR_patch_jmp_target(jmp_instr, loop_cond_check);

        // Emit condition expression.
        IR_Operand cond_op = {0};
        IR_emit_expr(builder, cond_expr, &cond_op);

        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP)
        {
            u32 loop_bottom = IR_get_jmp_target(builder);

            // Patch short-circuit jumps to the top or bottom of the loop.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next)
            {
                if (it->result)
                    IR_patch_jmp_target(it->jmp, loop_top);
                else
                    IR_patch_jmp_target(it->jmp, loop_bottom);
            }

            // Path final jump to the top of the loop (create one if jump does not exist).
            if (cond_op.cmp.final_jmp.jmp)
                IR_patch_jmp_target(cond_op.cmp.final_jmp.jmp, loop_top);
            else
                cond_op.cmp.final_jmp.jmp = IR_emit_instr_jmpcc(builder, cond_op.cmp.final_jmp.cond, loop_top);

            // Reverse jump condition so that it goes to the "true" path.
            if (!cond_op.cmp.final_jmp.result)
                cond_op.cmp.final_jmp.jmp->_jmpcc.cond = ir_opposite_cond[cond_op.cmp.final_jmp.cond];

        }
        else
        {
            IR_ensure_operand_in_reg(builder, &cond_op, true);

            IR_OpRM cond_arg = {.kind = IR_OP_REG, .reg = cond_op.reg};
            IR_OpRMI zero_arg = {.kind = IR_OP_IMM, .imm.as_int._u64 = 0};

            // Compare condition expression to zero and store result in a register.
            IR_emit_instr_cmp(builder, cond_op.type, cond_arg, zero_arg);

            // Emit conditional jump to the top of the loop.
            IR_emit_instr_jmpcc(builder, IR_COND_NEQ, loop_top);

            IR_try_free_op_reg(builder, &cond_op);
        }
    }
}

static void IR_emit_stmt_do_while(IR_Builder* builder, StmtDoWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_const)
    {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val)
        {
            u32 loop_top = IR_get_jmp_target(builder);

            // Emit loop body statements.
            IR_emit_stmt(builder, body);

            // Jump back to the top of the loop.
            IR_emit_instr_jmp(builder, loop_top);
        }
    }
    else
    {
        // Save the current instruction index to enable jumps to the top of the loop.
        u32 loop_top = IR_get_jmp_target(builder);

        // Emit instructions for the loop body.
        IR_emit_stmt(builder, body);

        // Emit condition expression.
        IR_Operand cond_op = {0};
        IR_emit_expr(builder, cond_expr, &cond_op);

        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP)
        {
            u32 loop_bottom = IR_get_jmp_target(builder);

            // Patch short-circuit jumps to the top or bottom of the loop.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next)
            {
                if (it->result)
                    IR_patch_jmp_target(it->jmp, loop_top);
                else
                    IR_patch_jmp_target(it->jmp, loop_bottom);
            }

            // Path final jump to the top of the loop (create one if jump does not exist).
            if (cond_op.cmp.final_jmp.jmp)
                IR_patch_jmp_target(cond_op.cmp.final_jmp.jmp, loop_top);
            else
                cond_op.cmp.final_jmp.jmp = IR_emit_instr_jmpcc(builder, cond_op.cmp.final_jmp.cond, loop_top);

            // Reverse jump condition so that it goes to the "true" path.
            if (!cond_op.cmp.final_jmp.result)
                cond_op.cmp.final_jmp.jmp->_jmpcc.cond = ir_opposite_cond[cond_op.cmp.final_jmp.cond];

        }
        else
        {
            IR_ensure_operand_in_reg(builder, &cond_op, true);

            IR_OpRM cond_arg = {.kind = IR_OP_REG, .reg = cond_op.reg};
            IR_OpRMI zero_arg = {.kind = IR_OP_IMM, .imm.as_int._u64 = 0};

            // Compare condition expression to zero and store result in a register.
            IR_emit_instr_cmp(builder, cond_op.type, cond_arg, zero_arg);

            // Emit conditional jump to the top of the loop.
            IR_emit_instr_jmpcc(builder, IR_COND_NEQ, loop_top);
        }

        IR_try_free_op_reg(builder, &cond_op);
    }
}

static void IR_emit_stmt(IR_Builder* builder, Stmt* stmt)
{
    switch (stmt->kind)
    {
        case CST_StmtBlock:
            IR_emit_stmt_block(builder, (StmtBlock*)stmt);
            break;
        case CST_StmtReturn:
            IR_emit_stmt_return(builder, (StmtReturn*)stmt);
            break;
        case CST_StmtDecl:
            IR_emit_stmt_decl(builder, (StmtDecl*)stmt);
            break;
        case CST_StmtExpr:
            IR_emit_stmt_expr(builder, (StmtExpr*)stmt);
            break;
        case CST_StmtExprAssign:
            IR_emit_stmt_expr_assign(builder, (StmtExprAssign*)stmt);
            break;
        case CST_StmtIf:
            IR_emit_stmt_if(builder, (StmtIf*)stmt);
            break;
        case CST_StmtWhile:
            IR_emit_stmt_while(builder, (StmtWhile*)stmt);
            break;
        case CST_StmtDoWhile:
            IR_emit_stmt_do_while(builder, (StmtDoWhile*)stmt);
            break;
        default:
            break;
    }
}

static u32 IR_assign_scope_var_offsets(Symbol*** proc_vars, Scope* scope, u32 offset)
{
    u32 stack_size = offset;

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
                Type* var_type = sym->type;
                size_t var_size = var_type->size;
                size_t var_align = var_type->align;

                stack_size += var_size;
                stack_size = ALIGN_UP(stack_size, var_align);
                sym->as_var.offset = -stack_size;

                array_push(*proc_vars, sym);
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
            u32 child_size = IR_assign_scope_var_offsets(proc_vars, child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, IR_STACK_ALIGN);
}

static void IR_assign_proc_var_offsets(IR_Builder* builder, Symbol* proc_sym)
{
    u32 stack_size = 0;
    DeclProc* dproc = (DeclProc*)proc_sym->decl;
    Scope* scope = dproc->scope;

    // Save allocator state so that the temporary arena's state can
    // be restored after building the temporary args array.
    AllocatorState arena_state = allocator_get_state(builder->tmp_arena);
    {
        // Create temporary array that will hold all args and local variables that belong to
        // this procedure. This includes variables declared in nested scopes.
        Symbol** proc_vars = array_create(builder->tmp_arena, Symbol*, dproc->num_params << 1);

        // Recursively assign a stack offset to each local variable (including proc arguments).
        // Variables declared in sibling scopes will be assigned to overlapping stack offsets
        // to use "less" stack space.
        stack_size = IR_assign_scope_var_offsets(&proc_vars, scope, stack_size);

        // Save the computed stack size and the vars array into the procedure symbol.
        u32 num_vars = (u32)array_len(proc_vars);

        proc_sym->as_proc.min_stack_size = ALIGN_UP(stack_size, IR_STACK_ALIGN);
        proc_sym->as_proc.num_vars = num_vars;
        proc_sym->as_proc.vars = alloc_array(builder->arena, Symbol*, num_vars, false);

        memcpy(proc_sym->as_proc.vars, proc_vars, num_vars * sizeof(Symbol*));
    }
    allocator_restore_state(arena_state);
}

static bool IR_build_proc(IR_Builder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    // Set procedure as the current scope.
    IR_push_scope(builder, dproc->scope);
    builder->curr_proc = sym;

    // Assign stack offsets to params and local vars.
    IR_assign_proc_var_offsets(builder, sym);

    sym->as_proc.instrs = array_create(builder->arena, IR_Instr*, 32);

    IR_emit_stmt_block_body(builder, &dproc->stmts);
    assert(builder->free_regs == (u32)-1);

    // If proc doesn't have explicit returns, add one at the end.
    // NOTE: This should only apply to procs that return void. The resolver
    // will catch other cases.
    if (!dproc->returns)
    {
        assert(sym->type->as_proc.ret == type_void);
        IR_emit_instr_ret(builder, type_void, IR_REG_COUNT);
    }

    IR_pop_scope(builder);
    builder->curr_proc = NULL;

#ifndef NDEBUG
    IR_print_out_proc(builder->tmp_arena, sym);
#endif

    return true;
}

IR_Module* IR_build_module(Allocator* arena, Allocator* tmp_arena, Scope* global_scope)
{
    IR_Module* module = alloc_type(arena, IR_Module, true);

    if (!module)
        return NULL;

    IR_Builder builder = {.arena = arena,
                          .tmp_arena = tmp_arena,
                          .curr_proc = NULL,
                          .curr_scope = global_scope,
                          .module = module,
                          .free_regs = -1};

    // Create global IR vars/procs arrays.
    module->num_vars = global_scope->sym_kind_counts[SYMBOL_VAR];
    module->vars = alloc_array(arena, Symbol*, module->num_vars, false);

    module->num_procs = global_scope->sym_kind_counts[SYMBOL_PROC];
    module->procs = alloc_array(arena, Symbol*, module->num_procs, false);

    // Iterate through all global declarations and create IR structures for
    // global variables and procedures.
    size_t var_index = 0;
    size_t proc_index = 0;

    List* head = &global_scope->sym_list;
    List* it = head->next;

    while (it != head)
    {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (sym->kind == SYMBOL_VAR)
        {
            module->vars[var_index] = sym;
            var_index += 1;
        }
        else if (sym->kind == SYMBOL_PROC)
        {
            module->procs[proc_index] = sym;
            proc_index += 1;
        }

        it = it->next;
    }

    assert(var_index == module->num_vars);
    assert(proc_index == module->num_procs);

    AllocatorState tmp_mem_state = allocator_get_state(builder.tmp_arena);

    // Iterate through all procedures and generate IR instructions.
    for (size_t i = 0; i < module->num_procs; i += 1)
    {
        IR_build_proc(&builder, module->procs[i]);
    }

    allocator_restore_state(tmp_mem_state);

    return module;
}
