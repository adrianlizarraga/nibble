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
    IR_OPERAND_MEM_ADDR,
    IR_OPERAND_DEREF_ADDR,
    IR_OPERAND_DEFERRED_CMP,
    IR_OPERAND_ARRAY_INIT,
    IR_OPERAND_VAR,
    IR_OPERAND_STR_LIT,
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

typedef struct IR_ArrayMemberInitializer IR_ArrayMemberInitializer;

typedef struct IR_ArrayInitializer {
    u64 num_initzers;
    IR_ArrayMemberInitializer* initzers;
} IR_ArrayInitializer;

typedef struct IR_Operand {
    IR_OperandKind kind;
    Type* type;

    union {
        Scalar imm;
        IR_Reg reg;
        IR_MemAddr addr;
        Symbol* sym;
        IR_DeferredCmp cmp;
        IR_ArrayInitializer array_initzer;
        StrLit* str_lit;
    };
} IR_Operand;

typedef struct IR_ArrayMemberInitializer {
    u64 index;
    IR_Operand op;
} IR_ArrayMemberInitializer;

typedef struct IR_Builder {
    Allocator* arena;
    Allocator* tmp_arena;
    TypeCache* type_cache;
    Symbol* curr_proc;
    Scope* curr_scope;

    bool next_instr_is_jmp_target;
    IR_DeferredJmpcc* sc_jmp_freelist;
    u32 free_regs;
} IR_Builder;

static const Scalar ir_zero_imm = {.as_int._u64 = 0};
static const Scalar ir_one_imm = {.as_int._u64 = 1};

static void IR_free_reg(IR_Builder* builder, IR_Reg reg)
{
    Symbol* sym = builder->curr_proc;
    LifetimeInterval* interval = &sym->as_proc.reg_intervals[reg];

    interval->end = array_len(sym->as_proc.instrs) - 1;
}

static IR_Reg IR_next_reg(IR_Builder* builder)
{
    Symbol* sym = builder->curr_proc;
    LifetimeInterval interval = {.start = array_len(sym->as_proc.instrs)};
    array_push(sym->as_proc.reg_intervals, interval);

    return array_len(sym->as_proc.reg_intervals) - 1;
}

static void IR_mark_reg_as_arg(IR_Builder* builder, IR_Reg reg, u32 arg_index)
{
    Symbol* sym = builder->curr_proc;
    LifetimeInterval* interval = &sym->as_proc.reg_intervals[reg];

    interval->is_arg = true;
    interval->arg_index = arg_index;
}

static void IR_mark_reg_as_ret(IR_Builder* builder, IR_Reg reg)
{
    Symbol* sym = builder->curr_proc;
    LifetimeInterval* interval = &sym->as_proc.reg_intervals[reg];

    interval->is_ret = true;
}

static void IR_try_free_op_reg(IR_Builder* builder, IR_Operand* op)
{
    switch (op->kind) {
    case IR_OPERAND_REG:
        IR_free_reg(builder, op->reg);
        break;
    case IR_OPERAND_DEREF_ADDR:
    case IR_OPERAND_MEM_ADDR: {
        IR_Reg base_reg = op->addr.base_kind == IR_MEM_BASE_REG ? op->addr.base.reg : IR_REG_COUNT;
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
    if (builder->next_instr_is_jmp_target) {
        instr->is_jmp_target = true;
        builder->next_instr_is_jmp_target = false;
    }

    array_push(builder->curr_proc->as_proc.instrs, instr);
}

static IR_Instr* IR_new_instr(Allocator* arena, IR_InstrKind kind)
{
    IR_Instr* instr = alloc_type(arena, IR_Instr, true);
    instr->kind = kind;

    return instr;
}

static void IR_emit_instr_add(IR_Builder* builder, Type* type, IR_Reg dst, IR_Operand* src_op)
{
    IR_Instr* instr = NULL;

    switch (src_op->kind) {
    case IR_OPERAND_REG:
        instr = IR_new_instr(builder->arena, IR_INSTR_ADD_R_R);
        instr->add_r_r.type = type;
        instr->add_r_r.dst = dst;
        instr->add_r_r.src = src_op->reg;
        break;
    case IR_OPERAND_MEM_ADDR:
        instr = IR_new_instr(builder->arena, IR_INSTR_ADD_R_M);
        instr->add_r_m.type = type;
        instr->add_r_m.dst = dst;
        instr->add_r_m.src = src_op->addr;
        break;
    case IR_OPERAND_VAR:
        instr = IR_new_instr(builder->arena, IR_INSTR_ADD_R_M);
        instr->add_r_m.type = type;
        instr->add_r_m.dst = dst;
        instr->add_r_m.src.base_kind = IR_MEM_BASE_SYM;
        instr->add_r_m.src.base.sym = src_op->sym;
        break;
    case IR_OPERAND_IMM:
        instr = IR_new_instr(builder->arena, IR_INSTR_ADD_R_I);
        instr->add_r_i.type = type;
        instr->add_r_i.dst = dst;
        instr->add_r_i.src = src_op->imm;
        break;
    default:
        assert(!0);
        break;
    }

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_add_r_i(IR_Builder* builder, Type* type, IR_Reg dst, Scalar src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_ADD_R_I);
    instr->add_r_i.type = type;
    instr->add_r_i.dst = dst;
    instr->add_r_i.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sub(IR_Builder* builder, Type* type, IR_Reg dst, IR_Operand* src_op)
{
    IR_Instr* instr = NULL;

    switch (src_op->kind) {
    case IR_OPERAND_REG:
        instr = IR_new_instr(builder->arena, IR_INSTR_SUB_R_R);
        instr->sub_r_r.type = type;
        instr->sub_r_r.dst = dst;
        instr->sub_r_r.src = src_op->reg;
        break;
    case IR_OPERAND_MEM_ADDR:
        instr = IR_new_instr(builder->arena, IR_INSTR_SUB_R_M);
        instr->sub_r_m.type = type;
        instr->sub_r_m.dst = dst;
        instr->sub_r_m.src = src_op->addr;
        break;
    case IR_OPERAND_VAR:
        instr = IR_new_instr(builder->arena, IR_INSTR_SUB_R_M);
        instr->sub_r_m.type = type;
        instr->sub_r_m.dst = dst;
        instr->sub_r_m.src.base_kind = IR_MEM_BASE_SYM;
        instr->sub_r_m.src.base.sym = src_op->sym;
        break;
    case IR_OPERAND_IMM:
        instr = IR_new_instr(builder->arena, IR_INSTR_SUB_R_I);
        instr->sub_r_i.type = type;
        instr->sub_r_i.dst = dst;
        instr->sub_r_i.src = src_op->imm;
        break;
    default:
        assert(!0);
        break;
    }

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_mul(IR_Builder* builder, Type* type, IR_Reg dst, IR_Operand* src_op)
{
    IR_Instr* instr = NULL;

    switch (src_op->kind) {
    case IR_OPERAND_REG:
        instr = IR_new_instr(builder->arena, IR_INSTR_MUL_R_R);
        instr->mul_r_r.type = type;
        instr->mul_r_r.dst = dst;
        instr->mul_r_r.src = src_op->reg;
        break;
    case IR_OPERAND_MEM_ADDR:
        instr = IR_new_instr(builder->arena, IR_INSTR_MUL_R_M);
        instr->mul_r_m.type = type;
        instr->mul_r_m.dst = dst;
        instr->mul_r_m.src = src_op->addr;
        break;
    case IR_OPERAND_VAR:
        instr = IR_new_instr(builder->arena, IR_INSTR_MUL_R_M);
        instr->mul_r_m.type = type;
        instr->mul_r_m.dst = dst;
        instr->mul_r_m.src.base_kind = IR_MEM_BASE_SYM;
        instr->mul_r_m.src.base.sym = src_op->sym;
        break;
    case IR_OPERAND_IMM:
        instr = IR_new_instr(builder->arena, IR_INSTR_MUL_R_I);
        instr->mul_r_i.type = type;
        instr->mul_r_i.dst = dst;
        instr->mul_r_i.src = src_op->imm;
        break;
    default:
        assert(!0);
        break;
    }

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_mul_r_i(IR_Builder* builder, Type* type, IR_Reg dst, Scalar src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_MUL_R_I);
    instr->mul_r_i.type = type;
    instr->mul_r_i.dst = dst;
    instr->mul_r_i.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_div(IR_Builder* builder, Type* type, IR_Reg dst, IR_Operand* src_op)
{
    assert(type->kind == TYPE_INTEGER);

    IR_Instr* instr = NULL;
    bool is_signed = type->as_integer.is_signed;

    switch (src_op->kind) {
    case IR_OPERAND_REG: {
        IR_InstrKind kind = is_signed ? IR_INSTR_SDIV_R_R : IR_INSTR_UDIV_R_R;
        instr = IR_new_instr(builder->arena, kind);
        instr->div_r_r.type = type;
        instr->div_r_r.dst = dst;
        instr->div_r_r.src = src_op->reg;
        break;
    }
    case IR_OPERAND_MEM_ADDR: {
        IR_InstrKind kind = is_signed ? IR_INSTR_SDIV_R_M : IR_INSTR_UDIV_R_M;
        instr = IR_new_instr(builder->arena, kind);
        instr->div_r_m.type = type;
        instr->div_r_m.dst = dst;
        instr->div_r_m.src = src_op->addr;
        break;
    }
    case IR_OPERAND_VAR: {
        IR_InstrKind kind = is_signed ? IR_INSTR_SDIV_R_M : IR_INSTR_UDIV_R_M;
        instr = IR_new_instr(builder->arena, kind);
        instr->div_r_m.type = type;
        instr->div_r_m.dst = dst;
        instr->div_r_m.src.base_kind = IR_MEM_BASE_SYM;
        instr->div_r_m.src.base.sym = src_op->sym;
        break;
    }
    case IR_OPERAND_IMM: {
        IR_InstrKind kind = is_signed ? IR_INSTR_SDIV_R_I : IR_INSTR_UDIV_R_I;
        instr = IR_new_instr(builder->arena, kind);
        instr->div_r_i.type = type;
        instr->div_r_i.dst = dst;
        instr->div_r_i.src = src_op->imm;
        break;
    }
    default:
        assert(!0);
        break;
    }

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sar(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_Operand* src_op)
{
    IR_Instr* instr = NULL;

    switch (src_op->kind) {
    case IR_OPERAND_REG:
        instr = IR_new_instr(builder->arena, IR_INSTR_SAR_R_R);
        instr->sar_r_r.dst_type = dst_type;
        instr->sar_r_r.src_type = src_type;
        instr->sar_r_r.dst = dst;
        instr->sar_r_r.src = src_op->reg;
        break;
    case IR_OPERAND_MEM_ADDR:
        instr = IR_new_instr(builder->arena, IR_INSTR_SAR_R_M);
        instr->sar_r_m.dst_type = dst_type;
        instr->sar_r_m.src_type = src_type;
        instr->sar_r_m.dst = dst;
        instr->sar_r_m.src = src_op->addr;
        break;
    case IR_OPERAND_VAR:
        instr = IR_new_instr(builder->arena, IR_INSTR_SAR_R_M);
        instr->sar_r_m.dst_type = dst_type;
        instr->sar_r_m.src_type = src_type;
        instr->sar_r_m.dst = dst;
        instr->sar_r_m.src.base_kind = IR_MEM_BASE_SYM;
        instr->sar_r_m.src.base.sym = src_op->sym;
        break;
    case IR_OPERAND_IMM:
        instr = IR_new_instr(builder->arena, IR_INSTR_SAR_R_I);
        instr->sar_r_i.dst_type = dst_type;
        instr->sar_r_i.src_type = src_type;
        instr->sar_r_i.dst = dst;
        instr->sar_r_i.src = src_op->imm;
        break;
    default:
        assert(!0);
        break;
    }

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sar_r_i(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, Scalar src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SAR_R_I);
    instr->sar_r_i.dst_type = dst_type;
    instr->sar_r_i.src_type = src_type;
    instr->sar_r_i.dst = dst;
    instr->sar_r_i.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_shl(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_Operand* src_op)
{
    IR_Instr* instr = NULL;

    switch (src_op->kind) {
    case IR_OPERAND_REG:
        instr = IR_new_instr(builder->arena, IR_INSTR_SHL_R_R);
        instr->shl_r_r.dst_type = dst_type;
        instr->shl_r_r.src_type = src_type;
        instr->shl_r_r.dst = dst;
        instr->shl_r_r.src = src_op->reg;
        break;
    case IR_OPERAND_MEM_ADDR:
        instr = IR_new_instr(builder->arena, IR_INSTR_SHL_R_M);
        instr->shl_r_m.dst_type = dst_type;
        instr->shl_r_m.src_type = src_type;
        instr->shl_r_m.dst = dst;
        instr->shl_r_m.src = src_op->addr;
        break;
    case IR_OPERAND_VAR:
        instr = IR_new_instr(builder->arena, IR_INSTR_SHL_R_M);
        instr->shl_r_m.dst_type = dst_type;
        instr->shl_r_m.src_type = src_type;
        instr->shl_r_m.dst = dst;
        instr->shl_r_m.src.base_kind = IR_MEM_BASE_SYM;
        instr->shl_r_m.src.base.sym = src_op->sym;
        break;
    case IR_OPERAND_IMM:
        instr = IR_new_instr(builder->arena, IR_INSTR_SHL_R_I);
        instr->shl_r_i.dst_type = dst_type;
        instr->shl_r_i.src_type = src_type;
        instr->shl_r_i.dst = dst;
        instr->shl_r_i.src = src_op->imm;
        break;
    default:
        assert(!0);
        break;
    }

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_neg(IR_Builder* builder, Type* type, IR_Reg dst)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_NEG);
    instr->neg.type = type;
    instr->neg.dst = dst;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_not(IR_Builder* builder, Type* type, IR_Reg dst)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_NOT);
    instr->not .type = type;
    instr->not .dst = dst;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_limm(IR_Builder* builder, Type* type, IR_Reg dst, Scalar src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LIMM);
    instr->limm.type = type;
    instr->limm.dst = dst;
    instr->limm.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_laddr(IR_Builder* builder, IR_Reg dst, Type* type, IR_MemAddr addr)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR);
    instr->laddr.dst = dst;
    instr->laddr.type = type;
    instr->laddr.mem = addr;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_laddr_sym(IR_Builder* builder, IR_Reg dst, Type* type, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR);
    instr->laddr.dst = dst;
    instr->laddr.type = type;
    instr->laddr.mem.base_kind = IR_MEM_BASE_SYM;
    instr->laddr.mem.base.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_laddr_str_lit(IR_Builder* builder, IR_Reg dst, Type* type, StrLit* str_lit)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR);
    instr->laddr.dst = dst;
    instr->laddr.type = type;
    instr->laddr.mem.base_kind = IR_MEM_BASE_STR_LIT;
    instr->laddr.mem.base.str_lit = str_lit;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_trunc_r_r(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_Reg src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_TRUNC_R_R);
    instr->trunc_r_r.dst_type = dst_type;
    instr->trunc_r_r.dst = dst;
    instr->trunc_r_r.src_type = src_type;
    instr->trunc_r_r.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_trunc_r_m(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_TRUNC_R_M);
    instr->trunc_r_m.dst_type = dst_type;
    instr->trunc_r_m.dst = dst;
    instr->trunc_r_m.src_type = src_type;
    instr->trunc_r_m.src.base_kind = IR_MEM_BASE_SYM;
    instr->trunc_r_m.src.base.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_zext_r_r(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_Reg src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_ZEXT_R_R);
    instr->zext_r_r.dst_type = dst_type;
    instr->zext_r_r.dst = dst;
    instr->zext_r_r.src_type = src_type;
    instr->zext_r_r.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_zext_r_m(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_ZEXT_R_M);
    instr->zext_r_m.dst_type = dst_type;
    instr->zext_r_m.dst = dst;
    instr->zext_r_m.src_type = src_type;
    instr->zext_r_m.src.base_kind = IR_MEM_BASE_SYM;
    instr->zext_r_m.src.base.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sext_r_r(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, IR_Reg src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SEXT_R_R);
    instr->sext_r_r.dst_type = dst_type;
    instr->sext_r_r.dst = dst;
    instr->sext_r_r.src_type = src_type;
    instr->sext_r_r.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sext_r_m(IR_Builder* builder, Type* dst_type, IR_Reg dst, Type* src_type, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SEXT_R_M);
    instr->sext_r_m.dst_type = dst_type;
    instr->sext_r_m.dst = dst;
    instr->sext_r_m.src_type = src_type;
    instr->sext_r_m.src.base_kind = IR_MEM_BASE_SYM;
    instr->sext_r_m.src.base.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_load(IR_Builder* builder, Type* type, IR_Reg dst, IR_MemAddr src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LOAD);
    instr->load.type = type;
    instr->load.dst = dst;
    instr->load.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_load_sym(IR_Builder* builder, Type* type, IR_Reg dst, Symbol* src_sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LOAD);
    instr->load.type = type;
    instr->load.dst = dst;
    instr->load.src.base_kind = IR_MEM_BASE_SYM;
    instr->load.src.base.sym = src_sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_store_r(IR_Builder* builder, Type* type, IR_MemAddr dst, IR_Reg src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_STORE_R);
    instr->store_r.type = type;
    instr->store_r.dst = dst;
    instr->store_r.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_store_i(IR_Builder* builder, Type* type, IR_MemAddr dst, Scalar src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_STORE_I);
    instr->store_i.type = type;
    instr->store_i.dst = dst;
    instr->store_i.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_cmp_r_r(IR_Builder* builder, Type* type, IR_Reg op1, IR_Reg op2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CMP_R_R);
    instr->cmp_r_r.type = type;
    instr->cmp_r_r.op1 = op1;
    instr->cmp_r_r.op2 = op2;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_cmp_r_m(IR_Builder* builder, Type* type, IR_Reg op1, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CMP_R_M);
    instr->cmp_r_m.type = type;
    instr->cmp_r_m.op1 = op1;
    instr->cmp_r_m.op2.base_kind = IR_MEM_BASE_SYM;
    instr->cmp_r_m.op2.base.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_cmp_r_i(IR_Builder* builder, Type* type, IR_Reg op1, Scalar op2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CMP_R_I);
    instr->cmp_r_i.type = type;
    instr->cmp_r_i.op1 = op1;
    instr->cmp_r_i.op2 = op2;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_cmp_m_r(IR_Builder* builder, Type* type, Symbol* sym, IR_Reg op2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CMP_M_R);
    instr->cmp_m_r.type = type;
    instr->cmp_m_r.op1.base_kind = IR_MEM_BASE_SYM;
    instr->cmp_m_r.op1.base.sym = sym;
    instr->cmp_m_r.op2 = op2;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_cmp_m_i(IR_Builder* builder, Type* type, Symbol* sym, Scalar op2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CMP_M_I);
    instr->cmp_m_i.type = type;
    instr->cmp_m_i.op1.base_kind = IR_MEM_BASE_SYM;
    instr->cmp_m_i.op1.base.sym = sym;
    instr->cmp_m_i.op2 = op2;

    IR_add_instr(builder, instr);
}

static IR_Instr* IR_emit_instr_jmp(IR_Builder* builder, u32 jmp_target)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_JMP);
    instr->jmp.jmp_target = jmp_target;

    IR_add_instr(builder, instr);

    return instr;
}

static IR_Instr* IR_emit_instr_jmpcc(IR_Builder* builder, IR_ConditionKind cond, u32 jmp_target)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_JMPCC);
    instr->jmpcc.cond = cond;
    instr->jmpcc.jmp_target = jmp_target;

    IR_add_instr(builder, instr);

    return instr;
}

static void IR_emit_instr_setcc(IR_Builder* builder, IR_ConditionKind cond, IR_Reg dst)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SETCC);
    instr->setcc.cond = cond;
    instr->setcc.dst = dst;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_ret(IR_Builder* builder, Type* type, IR_Reg src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_RET);
    instr->ret.type = type;
    instr->ret.src = src;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_call(IR_Builder* builder, Symbol* sym, IR_Reg dst, u32 num_args, IR_InstrCallArg* args)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CALL);
    instr->call.sym = sym;
    instr->call.dst = dst;
    instr->call.num_args = num_args;
    instr->call.args = args;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_call_r(IR_Builder* builder, Type* proc_type, IR_Reg proc_loc, IR_Reg dst, u32 num_args,
                                 IR_InstrCallArg* args)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CALL_R);
    instr->call_r.proc_type = proc_type;
    instr->call_r.proc_loc = proc_loc;
    instr->call_r.dst = dst;
    instr->call_r.num_args = num_args;
    instr->call_r.args = args;

    IR_add_instr(builder, instr);
}

static void IR_patch_jmp_target(IR_Instr* jmp_instr, u32 jmp_target)
{
    switch (jmp_instr->kind) {
    case IR_INSTR_JMP:
        jmp_instr->jmp.jmp_target = jmp_target;
        break;
    case IR_INSTR_JMPCC:
        jmp_instr->jmpcc.jmp_target = jmp_target;
        break;
    default:
        assert(0);
        break;
    }
}

static u32 IR_get_jmp_target(IR_Builder* builder)
{
    builder->next_instr_is_jmp_target = true;
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
    if (builder->sc_jmp_freelist) {
        new_node = builder->sc_jmp_freelist;
        builder->sc_jmp_freelist = new_node->next;
    }
    // Create a new node.
    else {
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
    if (!dst_cmp->first_sc_jmp) {
        assert(!dst_cmp->last_sc_jmp);
        dst_cmp->first_sc_jmp = src_cmp->first_sc_jmp;
        dst_cmp->last_sc_jmp = src_cmp->last_sc_jmp;
    }
    // Move non-empty source list to the end of the destination list.
    else if (src_cmp->first_sc_jmp) {
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

    if (dst_jmp->result != desired_result) {
        dst_jmp->cond = ir_opposite_cond[dst_jmp->cond];
        dst_jmp->result = desired_result;

        if (dst_jmp->jmp)
            dst_jmp->jmp->jmpcc.cond = dst_jmp->cond;
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

    if (!has_sc_jmps && !has_final_jmp) {
        IR_emit_instr_setcc(builder, def_cmp->final_jmp.cond, dst_reg);
        IR_emit_instr_zext_r_r(builder, operand->type, dst_reg, builtin_types[BUILTIN_TYPE_U8].type, dst_reg);
    }
    else {
        // Patch short-circuit jumps that jump to the "true" control path.
        for (IR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next) {
            if (it->result)
                IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
        }

        // This is the "true" control path. Move the literal 1 into destination register.
        IR_emit_instr_limm(builder, operand->type, dst_reg, ir_one_imm);

        // Patch short-circuit jumps that jump to the "false" control path.
        for (IR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next) {
            if (!it->result)
                IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
        }

        // Patch final jmp so that it jumps to "false" control path.
        IR_Instr* final_jmp = def_cmp->final_jmp.jmp;

        if (def_cmp->final_jmp.result)
            final_jmp->jmpcc.cond = ir_opposite_cond[final_jmp->jmpcc.cond];

        IR_patch_jmp_target(def_cmp->final_jmp.jmp, IR_get_jmp_target(builder));

        // This is the "false" control path. Move the literal 0 into destination register.
        IR_emit_instr_limm(builder, operand->type, dst_reg, ir_zero_imm);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_execute_deref(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEREF_ADDR);

    // The operand currently holds the address it is supposed to derefence.
    // This function executes the dereference into the one of the registers that held the address.
    IR_MemAddr addr = operand->addr;
    IR_Reg base_reg = addr.base_kind == IR_MEM_BASE_REG ? addr.base.reg : IR_REG_COUNT;
    IR_Reg index_reg = addr.scale ? addr.index_reg : IR_REG_COUNT;

    bool has_base_reg = base_reg < IR_REG_COUNT;
    bool has_index_reg = addr.index_reg < IR_REG_COUNT;

    IR_Reg dst_reg;

    if (has_base_reg)
        dst_reg = base_reg;
    else if (has_index_reg)
        dst_reg = index_reg;
    else
        dst_reg = IR_next_reg(builder);

    IR_emit_instr_load(builder, operand->type, dst_reg, addr);

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;

    if (has_base_reg && (base_reg != dst_reg))
        IR_free_reg(builder, base_reg);
    if (has_index_reg && (index_reg != dst_reg))
        IR_free_reg(builder, index_reg);
}

static void IR_execute_lea(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_MEM_ADDR);

    // The operand currently holds a memory address.
    // This function executes the "load-effective-address" call into the one of the registers that held the address.
    IR_MemAddr addr = operand->addr;
    IR_Reg base_reg = addr.base_kind == IR_MEM_BASE_REG ? addr.base.reg : IR_REG_COUNT;
    IR_Reg index_reg = addr.scale ? addr.index_reg : IR_REG_COUNT;

    bool has_base_reg = base_reg < IR_REG_COUNT;
    bool has_index_reg = addr.index_reg < IR_REG_COUNT;
    bool has_disp = addr.disp != 0;

    IR_Reg dst_reg;

    if (has_base_reg && !has_index_reg && !has_disp) {
        // No need to emit any instructions. Just keep address in base register.
        dst_reg = base_reg;
    }
    else {
        if (has_base_reg)
            dst_reg = base_reg;
        else if (has_index_reg)
            dst_reg = index_reg;
        else
            dst_reg = IR_next_reg(builder);

        IR_emit_instr_laddr(builder, dst_reg, operand->type, addr);

        if (has_base_reg && (base_reg != dst_reg))
            IR_free_reg(builder, base_reg);
        if (has_index_reg && (index_reg != dst_reg))
            IR_free_reg(builder, index_reg);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_op_to_r(IR_Builder* builder, IR_Operand* operand, bool commit_ptr)
{
    if (commit_ptr && (operand->kind == IR_OPERAND_MEM_ADDR)) {
        IR_execute_lea(builder, operand);
    }
    else if (operand->kind == IR_OPERAND_DEREF_ADDR) {
        IR_execute_deref(builder, operand);
    }
    else if (operand->kind == IR_OPERAND_DEFERRED_CMP) {
        IR_execute_deferred_cmp(builder, operand);
    }
    else if (operand->kind != IR_OPERAND_REG) {
        if (!commit_ptr && (operand->type->kind == TYPE_PTR)) {
            if (operand->kind != IR_OPERAND_MEM_ADDR) {
                IR_Reg base_reg = IR_next_reg(builder);
                IR_emit_instr_load_sym(builder, operand->type, base_reg, operand->sym);

                operand->kind = IR_OPERAND_MEM_ADDR;
                operand->addr.base_kind = IR_MEM_BASE_REG;
                operand->addr.base.reg = base_reg;
                operand->addr.index_reg = IR_REG_COUNT;
                operand->addr.scale = 0;
                operand->addr.disp = 0;
            }
        }
        else if (operand->kind == IR_OPERAND_IMM) {
            IR_Reg reg = IR_next_reg(builder);

            IR_emit_instr_limm(builder, operand->type, reg, operand->imm);

            operand->kind = IR_OPERAND_REG;
            operand->reg = reg;
        }
        else {
            IR_Reg reg = IR_next_reg(builder);
            IR_emit_instr_load_sym(builder, operand->type, reg, operand->sym);

            operand->kind = IR_OPERAND_REG;
            operand->reg = reg;
        }
    }
}

static void IR_op_to_rv(IR_Builder* builder, IR_Operand* op)
{
    switch (op->kind) {
    case IR_OPERAND_IMM: {
        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_limm(builder, op->type, reg, op->imm);

        op->kind = IR_OPERAND_REG;
        op->reg = reg;
        break;
    }
    case IR_OPERAND_DEREF_ADDR:
        IR_execute_deref(builder, op);
        break;
    case IR_OPERAND_DEFERRED_CMP:
        IR_execute_deferred_cmp(builder, op);
        break;
    case IR_OPERAND_MEM_ADDR:
        IR_execute_lea(builder, op);
        break;
    case IR_OPERAND_REG:
    case IR_OPERAND_VAR:
        // Do nothing.
        break;
    default:
        assert(!0);
        break;
    }
}

static void IR_op_to_ri(IR_Builder* builder, IR_Operand* op)
{
    switch (op->kind) {
    case IR_OPERAND_DEREF_ADDR:
        IR_execute_deref(builder, op);
        break;
    case IR_OPERAND_DEFERRED_CMP:
        IR_execute_deferred_cmp(builder, op);
        break;
    case IR_OPERAND_VAR: {
        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_load_sym(builder, op->type, reg, op->sym);

        op->kind = IR_OPERAND_REG;
        op->reg = reg;
        break;
    }
    case IR_OPERAND_MEM_ADDR:
        IR_execute_lea(builder, op);
        break;
    case IR_OPERAND_REG:
    case IR_OPERAND_IMM:
        // Do nothing.
        break;
    default:
        assert(!0);
        break;
    }
}

static void IR_op_to_rvi(IR_Builder* builder, IR_Operand* op)
{
    switch (op->kind) {
    case IR_OPERAND_DEREF_ADDR:
        IR_execute_deref(builder, op);
        break;
    case IR_OPERAND_DEFERRED_CMP:
        IR_execute_deferred_cmp(builder, op);
        break;
    case IR_OPERAND_MEM_ADDR:
        IR_execute_lea(builder, op);
        break;
    case IR_OPERAND_IMM:
    case IR_OPERAND_REG:
    case IR_OPERAND_VAR:
        // Do nothing.
        break;
    default:
        assert(!0);
        break;
    }
}

//////////////////////////////////////////////////////
//
//      Traverse AST to emit IR.
//
//////////////////////////////////////////////////////
static void IR_operand_from_sym(IR_Operand* op, Symbol* sym)
{
    if (sym->kind == SYMBOL_VAR) {
        op->kind = IR_OPERAND_VAR;
        op->type = sym->type;
        op->sym = sym;
    }
    else if (sym->kind == SYMBOL_PROC) {
        op->kind = IR_OPERAND_PROC;
        op->type = sym->type;
        op->sym = sym;
    }
    else {
        assert(0);
    }
}

static void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst);

static void IR_emit_expr_ident(IR_Builder* builder, ExprIdent* eident, IR_Operand* dst)
{
    Symbol* sym = NULL;

    if (eident->mod_ns) {
        Symbol* sym_modns = lookup_symbol(builder->curr_scope, eident->mod_ns);
        StmtImport* stmt = (StmtImport*)sym_modns->as_mod.stmt;

        Identifier* sym_name = get_import_sym_name(stmt, eident->name);

        sym = lookup_symbol(&sym_modns->as_mod.mod->scope, sym_name);
    }
    else {
        sym = lookup_symbol(builder->curr_scope, eident->name);
    }

    assert(sym);

    IR_operand_from_sym(dst, sym);
}

static void IR_emit_ptr_int_add(IR_Builder* builder, IR_Operand* dst, IR_Operand* ptr_op, IR_Operand* int_op, bool add)
{
    u64 base_size = ptr_op->type->as_ptr.base->size;

    IR_op_to_r(builder, ptr_op, false);

    if (int_op->kind == IR_OPERAND_IMM) {
        if (add)
            ptr_op->addr.disp += base_size * int_op->imm.as_int._u64;
        else
            ptr_op->addr.disp -= base_size * int_op->imm.as_int._u64;
    }
    else {
        if (ptr_op->addr.scale) {
            IR_op_to_rvi(builder, int_op);

            if (add)
                IR_emit_instr_add(builder, builtin_types[BUILTIN_TYPE_S64].type, ptr_op->addr.index_reg, int_op);
            else
                IR_emit_instr_sub(builder, builtin_types[BUILTIN_TYPE_S64].type, ptr_op->addr.index_reg, int_op);

            IR_try_free_op_reg(builder, int_op);
        }
        else {
            IR_op_to_r(builder, int_op, true);

            if (!add)
                IR_emit_instr_neg(builder, int_op->type, int_op->reg);

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

    IR_op_to_rvi(builder, right_op);

    if (right_op->kind == IR_OPERAND_VAR) {
        IR_op_to_ri(builder, left_op);

        switch (left_op->kind) {
        case IR_OPERAND_REG:
            IR_emit_instr_cmp_r_m(builder, left_op->type, left_op->reg, right_op->sym);
            break;
        case IR_OPERAND_IMM:
            // Flip operand order and reverse the condition.
            IR_emit_instr_cmp_m_i(builder, left_op->type, right_op->sym, left_op->imm);
            cond_kind = ir_opposite_cond[cond_kind];
            break;
        default:
            assert(0);
            break;
        }
    }
    else {
        IR_op_to_rv(builder, left_op);

        IR_OperandKind lkind = left_op->kind;
        IR_OperandKind rkind = right_op->kind;

        if (lkind == IR_OPERAND_REG && rkind == IR_OPERAND_REG) // Reg, Reg
            IR_emit_instr_cmp_r_r(builder, left_op->type, left_op->reg, right_op->reg);
        else if (lkind == IR_OPERAND_REG && rkind == IR_OPERAND_IMM) // Reg, Imm
            IR_emit_instr_cmp_r_i(builder, left_op->type, left_op->reg, right_op->imm);
        else if (lkind == IR_OPERAND_VAR && rkind == IR_OPERAND_REG) // Var, Reg
            IR_emit_instr_cmp_m_r(builder, left_op->type, left_op->sym, right_op->reg);
        else if (lkind == IR_OPERAND_VAR && rkind == IR_OPERAND_IMM) // Var, Imm
            IR_emit_instr_cmp_m_i(builder, left_op->type, left_op->sym, right_op->imm);
        else
            assert(0);
    }

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

    if (expr->op == TKN_LOGIC_AND) {
        short_circuit_val = false;
        short_circuit_cond = IR_COND_EQ;
    }
    else {
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
    if (left_op.kind == IR_OPERAND_DEFERRED_CMP) {
        // Copy list of short-circuit jumps.
        dst_op->cmp.first_sc_jmp = left_op.cmp.first_sc_jmp;
        dst_op->cmp.last_sc_jmp = left_op.cmp.last_sc_jmp;

        // Patch and remove short-circuit jumps with the opposite "short-circuit value".
        IR_DeferredJmpcc* it = dst_op->cmp.first_sc_jmp;
        IR_DeferredJmpcc* prev_it = NULL;

        while (it) {
            IR_DeferredJmpcc* next_it = it->next;

            if (it->result != short_circuit_val) {
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
    else {
        IR_op_to_rv(builder, &left_op);

        if (left_op.kind == IR_OPERAND_REG) {
            IR_emit_instr_cmp_r_i(builder, left_op.type, left_op.reg, ir_zero_imm);
            IR_free_reg(builder, left_op.reg);
        }
        else {
            assert(left_op.kind == IR_OPERAND_VAR);
            IR_emit_instr_cmp_m_i(builder, left_op.type, left_op.sym, ir_zero_imm);
        }

        IR_Instr* jmpcc_instr = IR_emit_instr_jmpcc(builder, short_circuit_cond, 0);

        IR_new_deferred_sc_jmp(builder, &dst_op->cmp, short_circuit_cond, short_circuit_val, jmpcc_instr);
    }

    // Emit instructions for the right expression.
    IR_emit_expr(builder, expr->right, &right_op);

    // If the right subexpression is a deferred comparison, merge into this deferred comparison result.
    // The right subexpression's short-circuit jumps are kept as-is.
    // The right subexpression's final jump is converted to a final jump to the "false" control path.
    if (right_op.kind == IR_OPERAND_DEFERRED_CMP) {
        // Merge lists of short-circuit jumps.
        IR_mov_deferred_sc_jmp_list(&dst_op->cmp, &right_op.cmp);

        // Convert the right expression's final jmp into a final jmp to the "false" path.
        IR_copy_sc_jmp(builder, &dst_op->cmp.final_jmp, &right_op.cmp.final_jmp, false);
    }
    // The right subexpression is some computation (not a deferred comparison). Compare the right subexpression to zero
    // and create a final jump.
    else {
        IR_op_to_rv(builder, &right_op);

        if (right_op.kind == IR_OPERAND_REG) {
            IR_emit_instr_cmp_r_i(builder, right_op.type, right_op.reg, ir_zero_imm);
            IR_free_reg(builder, right_op.reg);
        }
        else {
            assert(right_op.kind == IR_OPERAND_VAR);
            IR_emit_instr_cmp_m_i(builder, right_op.type, right_op.sym, ir_zero_imm);
        }

        dst_op->cmp.final_jmp.result = false;
        dst_op->cmp.final_jmp.jmp = IR_emit_instr_jmpcc(builder, IR_COND_EQ, 0);
        dst_op->cmp.final_jmp.cond = IR_COND_EQ;
    }
}

static void IR_emit_expr_binary(IR_Builder* builder, ExprBinary* expr, IR_Operand* dst)
{
    Type* result_type = expr->super.type;
    IR_Operand left = {0};
    IR_Operand right = {0};

    switch (expr->op) {
    case TKN_PLUS: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        bool left_is_ptr = left.type->kind == TYPE_PTR;
        bool right_is_ptr = right.type->kind == TYPE_PTR;

        if (left_is_ptr) {
            assert(result_type == left.type);
            IR_emit_ptr_int_add(builder, dst, &left, &right, true);
        }
        else if (right_is_ptr) {
            assert(result_type == right.type);
            IR_emit_ptr_int_add(builder, dst, &right, &left, true);
        }
        else {
            assert(left.type == right.type);
            assert(result_type == left.type);
            assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

            if (left.kind == IR_OPERAND_IMM) {
                IR_op_to_r(builder, &right, true);

                IR_emit_instr_add_r_i(builder, result_type, right.reg, left.imm);

                *dst = right;
            }
            else {
                IR_op_to_r(builder, &left, true);
                IR_op_to_rvi(builder, &right);

                IR_emit_instr_add(builder, result_type, left.reg, &right);

                *dst = left;
                IR_try_free_op_reg(builder, &right);
            }
        }
        break;
    }
    case TKN_MINUS: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        bool left_is_ptr = left.type->kind == TYPE_PTR;
        bool right_is_ptr = right.type->kind == TYPE_PTR;

        // ptr - int => ptr
        if (left_is_ptr && !right_is_ptr) {
            assert(result_type == left.type);
            IR_emit_ptr_int_add(builder, dst, &left, &right, false);
        }
        // ptr - ptr => s64
        else if (left_is_ptr && right_is_ptr) {
            assert(result_type == left.type);

            u64 base_size = left.type->as_ptr.base->size;
            u32 base_size_log2 = (u32)clp2(base_size);

            IR_op_to_r(builder, &left, true);
            IR_op_to_rvi(builder, &right);

            IR_emit_instr_sub(builder, result_type, left.reg, &right);

            if (base_size_log2) {
                Scalar shift_arg = {.as_int._u32 = base_size_log2};
                IR_emit_instr_sar_r_i(builder, result_type, left.reg, builtin_types[BUILTIN_TYPE_U8].type, shift_arg);
            }

            *dst = left;
            IR_try_free_op_reg(builder, &right);
        }
        // int - int => int
        else {
            assert(left.type == right.type);
            assert(result_type == left.type);
            assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

            IR_op_to_r(builder, &left, true);
            IR_op_to_rvi(builder, &right);

            IR_emit_instr_sub(builder, result_type, left.reg, &right);

            *dst = left;
            IR_try_free_op_reg(builder, &right);
        }
        break;
    }
    case TKN_ASTERISK: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        assert(left.type == right.type);
        assert(result_type == left.type);
        assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

        if (left.kind == IR_OPERAND_IMM) {
            IR_op_to_r(builder, &right, true);

            IR_emit_instr_mul_r_i(builder, result_type, right.reg, left.imm);

            *dst = right;
        }
        else {
            IR_op_to_r(builder, &left, true);
            IR_op_to_rvi(builder, &right);

            IR_emit_instr_mul(builder, result_type, left.reg, &right);

            *dst = left;
            IR_try_free_op_reg(builder, &right);
        }

        break;
    }
    case TKN_DIV: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        assert(left.type == right.type);
        assert(result_type == left.type);
        assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

        IR_op_to_r(builder, &left, true);
        IR_op_to_rvi(builder, &right);

        IR_emit_instr_div(builder, result_type, left.reg, &right);

        *dst = left;
        IR_try_free_op_reg(builder, &right);

        break;
    }
    case TKN_RSHIFT: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        assert(result_type == left.type);
        assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

        IR_op_to_r(builder, &left, true);
        IR_op_to_rvi(builder, &right);

        IR_emit_instr_sar(builder, result_type, left.reg, right.type, &right);

        *dst = left;
        IR_try_free_op_reg(builder, &right);
        break;
    }
    case TKN_LSHIFT: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        assert(result_type == left.type);
        assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

        IR_op_to_r(builder, &left, true);
        IR_op_to_rvi(builder, &right);

        IR_emit_instr_shl(builder, result_type, left.reg, right.type, &right);

        *dst = left;
        IR_try_free_op_reg(builder, &right);
        break;
    }
    case TKN_EQ: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);
        IR_emit_binary_cmp(builder, IR_COND_EQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_NOTEQ: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);
        IR_emit_binary_cmp(builder, IR_COND_NEQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_LT: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_LT : IR_COND_U_LT;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_LTEQ: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_LTEQ : IR_COND_U_LTEQ;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GT: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_GT : IR_COND_U_GT;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GTEQ: {
        IR_emit_expr(builder, expr->left, &left);
        IR_emit_expr(builder, expr->right, &right);

        IR_ConditionKind cond_kind = left.type->as_integer.is_signed ? IR_COND_S_GTEQ : IR_COND_U_GTEQ;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_LOGIC_AND:
    case TKN_LOGIC_OR: {
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

    switch (expr->op) {
    case TKN_PLUS: {
        IR_emit_expr(builder, expr->expr, dst);
        break;
    }
    case TKN_MINUS: // Two's compliment negation.
    {
        IR_emit_expr(builder, expr->expr, dst);
        IR_op_to_r(builder, dst, true);

        assert(dst->type == result_type);

        IR_emit_instr_neg(builder, result_type, dst->reg);
        break;
    }
    case TKN_NEG: // Bitwise not
    {
        IR_emit_expr(builder, expr->expr, dst);
        IR_op_to_r(builder, dst, true);

        assert(dst->type == result_type);

        IR_emit_instr_not(builder, result_type, dst->reg);
        break;
    }
    case TKN_NOT: // Logical not
    {
        dst->kind = IR_OPERAND_DEFERRED_CMP;
        dst->type = result_type;

        IR_Operand inner_op = {0};
        IR_emit_expr(builder, expr->expr, &inner_op);

        if (inner_op.kind == IR_OPERAND_DEFERRED_CMP) {
            // Reverse control paths for all jumps.
            // Ex: if a jmp instruction jumps to the "true" path, make it jump to the "false" path.
            dst->cmp.first_sc_jmp = inner_op.cmp.first_sc_jmp;
            dst->cmp.last_sc_jmp = inner_op.cmp.last_sc_jmp;

            for (IR_DeferredJmpcc* it = dst->cmp.first_sc_jmp; it; it = it->next) {
                it->result = !(it->result);
            }

            dst->cmp.final_jmp = inner_op.cmp.final_jmp;
            dst->cmp.final_jmp.result = !inner_op.cmp.final_jmp.result;
        }
        else {
            assert(inner_op.type == result_type);

            IR_op_to_rv(builder, &inner_op);

            if (inner_op.kind == IR_OPERAND_REG) {
                IR_emit_instr_cmp_r_i(builder, result_type, inner_op.reg, ir_zero_imm);
                IR_free_reg(builder, inner_op.reg);
            }
            else {
                assert(inner_op.kind == IR_OPERAND_VAR);
                IR_emit_instr_cmp_m_i(builder, result_type, inner_op.sym, ir_zero_imm);
            }

            dst->cmp.final_jmp.cond = IR_COND_EQ;
            dst->cmp.final_jmp.result = true;
            dst->cmp.first_sc_jmp = NULL;
            dst->cmp.last_sc_jmp = NULL;
            dst->cmp.final_jmp.jmp = NULL;
        }

        break;
    }
    case TKN_ASTERISK: {
        IR_emit_expr(builder, expr->expr, dst);
        IR_op_to_r(builder, dst, false);

        dst->kind = IR_OPERAND_DEREF_ADDR;
        dst->type = result_type;
        break;
    }
    case TKN_CARET: // Address-of operator
    {
        IR_emit_expr(builder, expr->expr, dst);

        if (dst->kind == IR_OPERAND_DEREF_ADDR) {
            dst->kind = IR_OPERAND_MEM_ADDR;
            dst->type = result_type;
        }
        else {
            assert(dst->kind == IR_OPERAND_VAR);

            IR_Reg result_reg = IR_next_reg(builder);

            IR_emit_instr_laddr_sym(builder, result_reg, dst->type, dst->sym);

            dst->kind = IR_OPERAND_MEM_ADDR;
            dst->type = result_type;
            dst->addr.base_kind = IR_MEM_BASE_REG;
            dst->addr.base.reg = result_reg;
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
    // We need the src expression to be a concrete value (Register, Variable).
    IR_op_to_rv(builder, src_op);

    IR_Reg dst_reg = IR_REG_COUNT;

    if (src_op->type->size == dst_op->type->size) {
        // This is a NO-OP even if any of the types is a ptr type.
        // Just make sure the result is in a register.
        if (src_op->kind == IR_OPERAND_REG) {
            dst_reg = src_op->reg;
        }
        else {
            assert(src_op->kind == IR_OPERAND_VAR);

            dst_reg = IR_next_reg(builder);
            IR_emit_instr_load_sym(builder, src_op->type, dst_reg, src_op->sym);
        }
    }
    // Truncate from larger type to smaller type.
    else if (src_op->type->size > dst_op->type->size) {
        dst_reg = IR_next_reg(builder);

        if (src_op->kind == IR_OPERAND_REG) {
            IR_emit_instr_trunc_r_r(builder, dst_op->type, dst_reg, src_op->type, src_op->reg);
            IR_free_reg(builder, src_op->reg);
        }
        else {
            assert(src_op->kind == IR_OPERAND_VAR);

            IR_emit_instr_trunc_r_m(builder, dst_op->type, dst_reg, src_op->type, src_op->sym);
        }
    }
    // Extend (sign or zero) to larger type.
    else {
        assert(src_op->type->size < dst_op->type->size);

        dst_reg = IR_next_reg(builder);
        bool src_signed = (src_op->type->kind == TYPE_INTEGER) && src_op->type->as_integer.is_signed;

        if (src_op->kind == IR_OPERAND_REG) {
            if (src_signed)
                IR_emit_instr_sext_r_r(builder, dst_op->type, dst_reg, src_op->type, src_op->reg);
            else
                IR_emit_instr_zext_r_r(builder, dst_op->type, dst_reg, src_op->type, src_op->reg);

            IR_free_reg(builder, src_op->reg);
        }
        else {
            assert(src_op->kind == IR_OPERAND_VAR);

            if (src_signed)
                IR_emit_instr_sext_r_m(builder, dst_op->type, dst_reg, src_op->type, src_op->sym);
            else
                IR_emit_instr_zext_r_m(builder, dst_op->type, dst_reg, src_op->type, src_op->sym);
        }
    }

    if (dst_op->type->kind == TYPE_PTR) {
        dst_op->kind = IR_OPERAND_MEM_ADDR;
        dst_op->addr.base_kind = IR_MEM_BASE_REG;
        dst_op->addr.base.reg = dst_reg;
        dst_op->addr.index_reg = IR_REG_COUNT;
        dst_op->addr.scale = 0;
        dst_op->addr.disp = 0;
    }
    else {
        dst_op->kind = IR_OPERAND_REG;
        dst_op->reg = dst_reg;
    }
}

static void IR_decay_array(IR_Builder* builder, IR_Operand* dst, Type* dst_type, IR_Operand* src)
{
    assert(src->type->kind == TYPE_ARRAY);
    assert(dst_type->kind == TYPE_PTR);

    if (src->kind == IR_OPERAND_VAR) {
        if (src->sym->is_local) {
            dst->addr.base_kind = IR_MEM_BASE_SYM;
            dst->addr.base.sym = src->sym;
        }
        else {
            IR_Reg dst_reg = IR_next_reg(builder);
            IR_emit_instr_laddr_sym(builder, dst_reg, src->type, src->sym);

            dst->addr.base_kind = IR_MEM_BASE_REG;
            dst->addr.base.reg = dst_reg;
        }
    }
    else {
        assert(src->kind == IR_OPERAND_STR_LIT);

        IR_Reg dst_reg = IR_next_reg(builder);
        IR_emit_instr_laddr_str_lit(builder, dst_reg, src->type, src->str_lit);

        dst->addr.base_kind = IR_MEM_BASE_REG;
        dst->addr.base.reg = dst_reg;
    }

    dst->type = dst_type;
    dst->kind = IR_OPERAND_MEM_ADDR;
    dst->addr.index_reg = IR_REG_COUNT;
    dst->addr.disp = 0;
    dst->addr.scale = 0;
}

static void IR_emit_expr_cast(IR_Builder* builder, ExprCast* expr_cast, IR_Operand* dst_op)
{
    // Emit instructions for source expression that will be casted.
    IR_Operand src_op = {0};
    IR_emit_expr(builder, expr_cast->expr, &src_op);

    dst_op->type = expr_cast->super.type;

    // TODO: Support floats.
    assert(src_op.type->kind != TYPE_FLOAT);
    assert(dst_op->type->kind != TYPE_FLOAT);
    assert(src_op.type != dst_op->type); // Should be prevented by resolver.

    if (src_op.type->kind == TYPE_ARRAY && dst_op->type->kind == TYPE_PTR) {
        IR_decay_array(builder, dst_op, dst_op->type, &src_op);
    }
    else {
        IR_emit_int_cast(builder, &src_op, dst_op);
    }
}

static bool IR_type_fits_in_reg(Type* type)
{
    return type->size <= PTR_SIZE;
}

static IR_InstrCallArg* IR_setup_call_args(IR_Builder* builder, ExprCall* expr_call)
{
    u32 num_args = (u32)expr_call->num_args;
    IR_InstrCallArg* args = alloc_array(builder->arena, IR_InstrCallArg, num_args, false);

    // Emit instructions for each argument expression and collect the resulting expression values
    // into an `args` array.
    u32 arg_index = 0;
    List* head = &expr_call->args;
    List* it = head->next;

    while (it != head) {
        ProcCallArg* ast_arg = list_entry(it, ProcCallArg, lnode);
        IR_Operand arg_op = {0};

        IR_emit_expr(builder, ast_arg->expr, &arg_op);

        if (IR_type_fits_in_reg(arg_op.type)) {
            IR_op_to_r(builder, &arg_op, true);
            IR_mark_reg_as_arg(builder, arg_op.reg, arg_index);

            assert(arg_index < num_args);
            args[arg_index].type = arg_op.type;
            args[arg_index].loc = arg_op.reg;
        }
        else {
            // TODO: Support struct types
            assert(0);
        }

        arg_index += 1;
        it = it->next;
    }

    return args;
}

static void IR_setup_call_ret(IR_Builder* builder, ExprCall* expr_call, IR_Operand* dst_op)
{
    dst_op->type = expr_call->super.type;

    // Allocate register if procedure returns a value.
    if (dst_op->type != builtin_types[BUILTIN_TYPE_VOID].type) {
        if (IR_type_fits_in_reg(dst_op->type)) {
            dst_op->kind = IR_OPERAND_REG;
            dst_op->reg = IR_next_reg(builder);
        }
        else {
            // TODO: Support returning structs
            assert(0);
        }
    }
    else {
        dst_op->kind = IR_OPERAND_NONE;
        dst_op->reg = IR_REG_COUNT;
    }
}

static void IR_cleanup_call_args(IR_Builder* builder, u32 num_args, IR_InstrCallArg* args)
{
    for (u32 i = 0; i < num_args; i += 1) {
        IR_free_reg(builder, args[i].loc);
    }
}

static void IR_emit_expr_call(IR_Builder* builder, ExprCall* expr_call, IR_Operand* dst_op)
{
    u32 num_args = (u32)expr_call->num_args;
    IR_InstrCallArg* args = IR_setup_call_args(builder, expr_call);

    // Emit instructions for the procedure pointer/name.
    IR_Operand proc_op = {0};
    IR_emit_expr(builder, expr_call->proc, &proc_op);

    // Allocate register for return value, emit call instruction, and then cleanup.
    if (proc_op.kind == IR_OPERAND_PROC) {
        // Direct procedure call.
        IR_setup_call_ret(builder, expr_call, dst_op);
        IR_emit_instr_call(builder, proc_op.sym, dst_op->reg, num_args, args);
        IR_cleanup_call_args(builder, num_args, args);
    }
    else {
        // Indirect procedure call through register.
        IR_op_to_r(builder, &proc_op, true);
        IR_setup_call_ret(builder, expr_call, dst_op);
        IR_emit_instr_call_r(builder, proc_op.type, proc_op.reg, dst_op->reg, num_args, args);
        IR_cleanup_call_args(builder, num_args, args);
        IR_free_reg(builder, proc_op.reg);
    }

    // Mark current procedure as non-leaf. This probably doesn't belong here, but I will probably delete/replace all of
    // this code.
    builder->curr_proc->as_proc.is_nonleaf = true;
}

static void IR_emit_expr_compound_lit(IR_Builder* builder, ExprCompoundLit* expr, IR_Operand* dst)
{
    // TODO: Currently only support array initializers.
    assert(expr->super.type->kind == TYPE_ARRAY);
    assert(!expr->typespec);

    u64 initzer_index = 0;
    IR_ArrayMemberInitializer* ir_initzers =
        alloc_array(builder->tmp_arena, IR_ArrayMemberInitializer, expr->num_initzers, true);

    List* head = &expr->initzers;
    List* it = head->next;
    u64 elem_index = 0;

    while (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        IR_ArrayMemberInitializer* ir_initzer = ir_initzers + initzer_index;

        if (initzer->designator.kind == DESIGNATOR_INDEX) {
            IR_Operand desig_op = {0};
            IR_emit_expr(builder, initzer->designator.index, &desig_op);

            assert(desig_op.kind == IR_OPERAND_IMM);
            elem_index = desig_op.imm.as_int._u64;
        }
        else {
            assert(initzer->designator.kind == DESIGNATOR_NONE);
        }

        ir_initzer->index = elem_index;
        IR_emit_expr(builder, initzer->init, &ir_initzer->op);

        elem_index += 1;
        initzer_index += 1;
        it = it->next;
    }

    dst->kind = IR_OPERAND_ARRAY_INIT;
    dst->type = expr->super.type;
    dst->array_initzer.num_initzers = expr->num_initzers;
    dst->array_initzer.initzers = ir_initzers;
}

static void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst)
{
    if (expr->is_constexpr && type_is_scalar(expr->type)) {
        dst->kind = IR_OPERAND_IMM;
        dst->type = expr->type;
        dst->imm = expr->const_val;

        return;
    }

    switch (expr->kind) {
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
    case CST_ExprCompoundLit:
        IR_emit_expr_compound_lit(builder, (ExprCompoundLit*)expr, dst);
        break;
    case CST_ExprStr: {
        ExprStr* expr_str_lit = (ExprStr*)expr;

        dst->kind = IR_OPERAND_STR_LIT;
        dst->type = expr_str_lit->super.type;
        dst->str_lit = expr_str_lit->str_lit;

        break;
    }
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

static void IR_emit_assign(IR_Builder* builder, IR_Operand* lhs, IR_Operand* rhs);

// Emit code for initializing an array with an initializer.
//    var a: [11] int = {0, 1, 2, 3};
static void IR_emit_array_init(IR_Builder* builder, IR_Operand* array_op, IR_Operand* init_op)
{
    assert(array_op->kind == IR_OPERAND_VAR);
    assert(init_op->kind == IR_OPERAND_ARRAY_INIT);
    assert(array_op->type->kind == TYPE_ARRAY);

    Type* arr_type = array_op->type;
    Type* ptr_type = type_decay(builder->arena, &builder->type_cache->ptrs, arr_type);
    Type* elem_type = ptr_type->as_ptr.base;

    // Decay array into pointer to the first elem.
    IR_Operand base_ptr_op = {0};
    IR_decay_array(builder, &base_ptr_op, ptr_type, array_op);

    IR_ArrayMemberInitializer* initzers = init_op->array_initzer.initzers;
    u64 num_initzers = init_op->array_initzer.num_initzers;
    u64 num_elems = arr_type->as_array.len;

    // Create array of bit flags: 1 bit per element in array.
    // Bit will be set to 1 if the array element has an initializer.
    const int num_bits = sizeof(size_t) * 8;
    size_t num_flags = (num_elems + num_bits - 1) / num_bits;
    size_t* init_flags = alloc_array(builder->tmp_arena, size_t, num_flags, true);

    // Iterate through initializers and: 1. mark element as having an initializer, 2. initialize element.
    for (size_t i = 0; i < num_initzers; i += 1) {
        IR_ArrayMemberInitializer* initzer = initzers + i;
        size_t elem_index = initzer->index;

        // Mark array element as having an initializer.
        size_t flag_index = elem_index / num_bits;
        size_t bit_index = elem_index % num_bits;
        size_t* flag = init_flags + flag_index;

        *flag |= (1 << bit_index);

        // Initialize array element with value of the initializer.
        IR_Operand elem_ptr_op = {.kind = IR_OPERAND_DEREF_ADDR, .type = elem_type, .addr = base_ptr_op.addr};
        elem_ptr_op.addr.disp += elem_type->size * elem_index;

        IR_emit_assign(builder, &elem_ptr_op, &initzer->op);
    }

    // For each array element, compute the pointer to the corresponding element and assign it
    // an default value if not yet initialized.
    IR_Operand zero_op = {.kind = IR_OPERAND_IMM, .type = elem_type, .imm = ir_zero_imm};

    for (u64 elem_index = 0; elem_index < num_elems; elem_index += 1) {
        size_t flag_index = elem_index / num_bits;
        size_t bit_index = elem_index % num_bits;

        // Skip array elements that have been initialized.
        if (init_flags[flag_index] & (1 << bit_index)) {
            continue;
        }

        IR_Operand elem_ptr_op = {.kind = IR_OPERAND_DEREF_ADDR, .type = elem_type, .addr = base_ptr_op.addr};
        elem_ptr_op.addr.disp += elem_type->size * elem_index;

        IR_emit_assign(builder, &elem_ptr_op, &zero_op);
    }

    // TODO: Reduce the number of assignment (mov) instructions by initializing
    // multiple elements at a time (one machine word's worth).
}

// Emit code for initializing an array with a string literal (is a copy of string literal).
//    var a: [6] char = "Hello";
//
//    Equivalent to:
//
//    var a: [6] char = {'H', 'e', 'l', 'l', 'o', '\0'};
static void IR_emit_array_str_init(IR_Builder* builder, IR_Operand* array_op, IR_Operand* init_op)
{
    assert(array_op->kind == IR_OPERAND_VAR);
    assert(init_op->kind == IR_OPERAND_STR_LIT);
    assert(array_op->type->kind == TYPE_ARRAY);

    Type* arr_type = array_op->type;
    Type* ptr_type = type_decay(builder->arena, &builder->type_cache->ptrs, arr_type);
    Type* elem_type = ptr_type->as_ptr.base;
    u64 num_elems = arr_type->as_array.len;

    StrLit* str_lit = init_op->str_lit;
    const char* str = str_lit->str;

    assert((str_lit->len + 1) == num_elems);

    // Decay array into pointer to the first elem.
    IR_Operand base_ptr_op = {0};
    IR_decay_array(builder, &base_ptr_op, ptr_type, array_op);

    for (u64 elem_index = 0; elem_index < num_elems; elem_index += 1) {
        IR_Operand char_op = {.kind = IR_OPERAND_IMM, .type = elem_type, .imm.as_int._u64 = str[elem_index]};

        IR_Operand elem_ptr_op = {.kind = IR_OPERAND_DEREF_ADDR, .type = elem_type, .addr = base_ptr_op.addr};
        elem_ptr_op.addr.disp += elem_type->size * elem_index;

        IR_emit_assign(builder, &elem_ptr_op, &char_op);
    }

    // TODO: Reduce the number of assignment (mov) instructions by initializing
    // multiple elements at a time (one machine word's worth).
}

static void IR_emit_assign(IR_Builder* builder, IR_Operand* lhs, IR_Operand* rhs)
{
    switch (lhs->kind) {
    case IR_OPERAND_VAR: {
        IR_MemAddr var_addr = {.base_kind = IR_MEM_BASE_SYM, .base.sym = lhs->sym};

        if (rhs->kind == IR_OPERAND_IMM) {
            IR_emit_instr_store_i(builder, lhs->type, var_addr, rhs->imm);
        }
        else if (rhs->kind == IR_OPERAND_ARRAY_INIT) {
            IR_emit_array_init(builder, lhs, rhs);
        }
        else if (rhs->kind == IR_OPERAND_STR_LIT) {
            IR_emit_array_str_init(builder, lhs, rhs);
        }
        else {
            IR_op_to_r(builder, rhs, true);
            IR_emit_instr_store_r(builder, lhs->type, var_addr, rhs->reg);
        }

        break;
    }
    case IR_OPERAND_DEREF_ADDR: {
        if (rhs->kind == IR_OPERAND_IMM) {
            IR_emit_instr_store_i(builder, lhs->type, lhs->addr, rhs->imm);
        }
        else {
            IR_op_to_r(builder, rhs, true);
            IR_emit_instr_store_r(builder, lhs->type, lhs->addr, rhs->reg);
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

    while (it != head) {
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
    IR_op_to_r(builder, &expr_op, true);

    IR_emit_instr_ret(builder, expr_op.type, expr_op.reg);

    IR_mark_reg_as_ret(builder, expr_op.reg);
    IR_free_reg(builder, expr_op.reg);
}

static void IR_emit_stmt_expr(IR_Builder* builder, StmtExpr* sexpr)
{
    IR_Operand expr_op = {0};
    IR_emit_expr(builder, sexpr->expr, &expr_op);
    IR_op_to_rvi(builder, &expr_op);
    IR_try_free_op_reg(builder, &expr_op);
}

static void IR_emit_stmt_expr_assign(IR_Builder* builder, StmtExprAssign* stmt)
{
    switch (stmt->op_assign) {
    case TKN_ASSIGN: {
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

    if (dvar->init) {
        IR_Operand rhs_op = {0};
        IR_Operand lhs_op = {0};

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
    if (cond_expr->is_constexpr && type_is_scalar(cond_expr->type)) {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        if (cond_val)
            IR_emit_stmt(builder, if_body);
        else
            IR_emit_stmt(builder, else_body);
    }
    else {
        IR_Operand cond_op = {0};
        IR_emit_expr(builder, cond_expr, &cond_op);

        IR_Instr* jmpcc_false = NULL;

        // If the condition is a chain of deferred comparisons, patch the jump targets
        // for all short-circuit jumps that jump to the "true" path to the
        // current instruction index.
        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP) {
            // Patch the jump target for all short-circuit jmps that jump to the "true" control path.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next) {
                if (it->result)
                    IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
            }

            // Create a jump to the "false" path if it doesn't yet exist.
            if (!cond_op.cmp.final_jmp.jmp)
                cond_op.cmp.final_jmp.jmp = IR_emit_instr_jmpcc(builder, cond_op.cmp.final_jmp.cond, 0);
        }
        // If the condition is some computation, compare the condition to zero and create a conditional
        // jump to the "false" path.
        else {
            IR_op_to_r(builder, &cond_op, true);
            IR_emit_instr_cmp_r_i(builder, cond_op.type, cond_op.reg, ir_zero_imm);

            // Emit conditional jump without a jump target. The jump target will be filled in below.
            jmpcc_false = IR_emit_instr_jmpcc(builder, IR_COND_EQ, 0);

            IR_free_reg(builder, cond_op.reg);
        }

        // Emit instructions for if-block body.
        IR_emit_stmt(builder, if_body);

        // Code path from if-block needs to jump over the else block. However, this jump instruction is only necessary
        // if a non-empty else block exists and if not all code paths within the if-block return.
        IR_Instr* jmp_end_instr = else_body && !if_body->returns ? IR_emit_instr_jmp(builder, 0) : NULL;

        // Patch conditional jmp instruction(s) that jump over the if-block when the condition is false.
        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP) {
            // Patch the jump target for all short-circuit jmps that jump to the "false" control path.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next) {
                if (!it->result)
                    IR_patch_jmp_target(it->jmp, IR_get_jmp_target(builder));
            }

            // Patch final jmp to "false" control path.
            if (cond_op.cmp.final_jmp.result)
                cond_op.cmp.final_jmp.jmp->jmpcc.cond = ir_opposite_cond[cond_op.cmp.final_jmp.cond];

            IR_patch_jmp_target(cond_op.cmp.final_jmp.jmp, IR_get_jmp_target(builder));
        }
        else {
            IR_patch_jmp_target(jmpcc_false, IR_get_jmp_target(builder));
        }

        if (else_body) {
            // Emit instructions for else-block body.
            IR_emit_stmt(builder, else_body);

            // Patch jmp instruction that jumps to the end of the else-block.
            if (jmp_end_instr)
                IR_patch_jmp_target(jmp_end_instr, IR_get_jmp_target(builder));
        }
    }
}

static void IR_emit_stmt_while(IR_Builder* builder, StmtWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_constexpr && type_is_scalar(cond_expr->type)) {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val) {
            u32 loop_top = IR_get_jmp_target(builder);

            // Emit loop body statements.
            IR_emit_stmt(builder, body);

            // Jump back to the top of the loop.
            IR_emit_instr_jmp(builder, loop_top);
        }
    }
    else {
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

        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP) {
            u32 loop_bottom = IR_get_jmp_target(builder);

            // Patch short-circuit jumps to the top or bottom of the loop.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next) {
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
                cond_op.cmp.final_jmp.jmp->jmpcc.cond = ir_opposite_cond[cond_op.cmp.final_jmp.cond];
        }
        else {
            IR_op_to_r(builder, &cond_op, true);

            // Compare condition expression to zero.
            IR_emit_instr_cmp_r_i(builder, cond_op.type, cond_op.reg, ir_zero_imm);

            // Emit conditional jump to the top of the loop.
            IR_emit_instr_jmpcc(builder, IR_COND_NEQ, loop_top);

            IR_free_reg(builder, cond_op.reg);
        }
    }
}

static void IR_emit_stmt_do_while(IR_Builder* builder, StmtDoWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_constexpr && type_is_scalar(cond_expr->type)) {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val) {
            u32 loop_top = IR_get_jmp_target(builder);

            // Emit loop body statements.
            IR_emit_stmt(builder, body);

            // Jump back to the top of the loop.
            IR_emit_instr_jmp(builder, loop_top);
        }
    }
    else {
        // Save the current instruction index to enable jumps to the top of the loop.
        u32 loop_top = IR_get_jmp_target(builder);

        // Emit instructions for the loop body.
        IR_emit_stmt(builder, body);

        // Emit condition expression.
        IR_Operand cond_op = {0};
        IR_emit_expr(builder, cond_expr, &cond_op);

        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP) {
            u32 loop_bottom = IR_get_jmp_target(builder);

            // Patch short-circuit jumps to the top or bottom of the loop.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next) {
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
                cond_op.cmp.final_jmp.jmp->jmpcc.cond = ir_opposite_cond[cond_op.cmp.final_jmp.cond];
        }
        else {
            IR_op_to_r(builder, &cond_op, true);

            // Compare condition expression to zero.
            IR_emit_instr_cmp_r_i(builder, cond_op.type, cond_op.reg, ir_zero_imm);

            // Emit conditional jump to the top of the loop.
            IR_emit_instr_jmpcc(builder, IR_COND_NEQ, loop_top);

            IR_free_reg(builder, cond_op.reg);
        }
    }
}

static void IR_emit_stmt(IR_Builder* builder, Stmt* stmt)
{
    switch (stmt->kind) {
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
    case CST_StmtStaticAssert:
        // Do nothing.
        break;
    default:
        ftprint_err("Cannot emit bytecode instruction for statement kind `%d`\n", stmt->kind);
        assert(0);
        break;
    }
}

static void IR_build_proc(IR_Builder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    if (sym->decl->flags & DECL_IS_INCOMPLETE) {
        return;
    }

    // Set procedure as the current scope.
    IR_push_scope(builder, dproc->scope);
    builder->curr_proc = sym;

    sym->as_proc.instrs = array_create(builder->arena, IR_Instr*, 32);
    sym->as_proc.reg_intervals = array_create(builder->arena, LifetimeInterval, 16);

    IR_emit_stmt_block_body(builder, &dproc->stmts);
    assert(builder->free_regs == (u32)-1);

    // If proc doesn't have explicit returns, add one at the end.
    // NOTE: This should only apply to procs that return void. The resolver
    // will catch other cases.
    if (!dproc->returns) {
        assert(sym->type->as_proc.ret == builtin_types[BUILTIN_TYPE_VOID].type);
        IR_emit_instr_ret(builder, builtin_types[BUILTIN_TYPE_VOID].type, IR_REG_COUNT);
    }

    IR_pop_scope(builder);
    builder->curr_proc = NULL;

    IR_print_out_proc(builder->tmp_arena, sym);
#if 0
    ftprint_out("Lifetime intervals:\n");
    LifetimeInterval* intervals = sym->as_proc.reg_intervals;
    u32 num_intervals = array_len(intervals);
    for (u32 i = 0; i < num_intervals; i += 1)
    {
        if (intervals[i].is_arg)
            ftprint_out("\t%u | %u -> %u (arg: %u)\n", i, intervals[i].start, intervals[i].end, intervals[i].arg_index);
        else
            ftprint_out("\t%u | %u -> %u\n", i, intervals[i].start, intervals[i].end);
    }
#endif
}

void IR_gen_bytecode(Allocator* arena, Allocator* tmp_arena, BucketList* procs, TypeCache* type_cache)
{
    IR_Builder builder = {.arena = arena,
                          .tmp_arena = tmp_arena,
                          .type_cache = type_cache,
                          .curr_proc = NULL,
                          .curr_scope = NULL,
                          .free_regs = -1};

    AllocatorState tmp_mem_state = allocator_get_state(builder.tmp_arena);

    // Iterate through all procedures and generate IR instructions.
    size_t num_procs = procs->num_elems;

    for (size_t i = 0; i < num_procs; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_PROC);

        IR_build_proc(&builder, sym);
    }

    allocator_restore_state(tmp_mem_state);
}
