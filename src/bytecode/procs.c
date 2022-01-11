#include "bytecode.h"
#include "print_ir.h"

typedef struct IR_ProcBuilder {
    Allocator* arena;
    Allocator* tmp_arena;
    TypeCache* type_cache;
    Symbol* curr_proc;
    Scope* curr_scope;

    bool next_instr_is_jmp_target;
    struct IR_DeferredJmpcc* sc_jmp_freelist;
} IR_ProcBuilder;

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
    bool result;
    Instr* jmp; // target needs to be patched. Cond reg needs to be set.
    Instr* cmp; // Condition may need to be inverted.
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
        MemAddr addr;
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

static const Scalar ir_zero_imm = {.as_int._u64 = 0};
static const Scalar ir_one_imm = {.as_int._u64 = 1};

static const ConditionKind ir_opposite_cond[] = {
    [COND_U_LT] = COND_U_GTEQ, [COND_S_LT] = COND_S_GTEQ, [COND_U_LTEQ] = COND_U_GT, [COND_S_LTEQ] = COND_S_GT,
    [COND_U_GT] = COND_U_LTEQ, [COND_S_GT] = COND_S_LTEQ, [COND_U_GTEQ] = COND_U_LT, [COND_S_GTEQ] = COND_S_LT,
    [COND_EQ] = COND_NEQ,      [COND_NEQ] = COND_EQ,
};

//////////////////////////////////////////////////////
//
//         Create IR instructions
//
//////////////////////////////////////////////////////
static void IR_bblock_add_instr(BBlock* bblock, Instr* instr)
{
    if (!bblock->first) {
        bblock->first = instr;
    }
    else {
        bblock->last->next = instr;
    }

    instr->prev = bblock->last;
    bblock->last = instr;

    bblock->num_instrs += 1;
}

static BBlock* IR_alloc_bblock(IR_ProcBuilder* builder)
{
    BBlock* block = alloc_type(builder->arena, Block, true);

    block->id = array_len(builder->curr_proc.as_proc.bblocks);
    block->preds = array_create(builder->arena, BBlock*, 4);
    block->succs = array_create(builder->arena, BBlock*, 4);

    array_push(builder->curr_proc.as_proc.bblocks, block);

    return block;
}

static void IR_connect_bblocks(BBlock* pred, BBlock* succ)
{
    array_push(pred->succs, succ);
    array_push(succ->preds, pred);
}

static void IR_patch_jmp_target(Instr* jmp_instr, BBlock* target)
{
    switch (jmp_instr->kind) {
    case INSTR_JMP:
        jmp_instr->jmp.target = target;
        break;
    case INSTR_COND_JMP:
        jmp_instr->cond_jmp.true_bb = target; // NOTE: Only patches true path
        break;
    default:
        assert(0);
        break;
    }
}

static void IR_add_instr(IR_ProcBuilder* builder, BBlock* bblock, Instr* instr)
{
    if (builder->next_instr_is_jmp_target) {
        instr->is_jmp_target = true;
        builder->next_instr_is_jmp_target = false;
    }

    IR_bblock_add_instr(bblock, instr);
}

static Instr* IR_new_instr(Allocator* arena, InstrKind kind)
{
    Instr* instr = alloc_type(arena, Instr, true);
    instr->kind = kind;

    return instr;
}

#define IR_emit_instr_add(bld, blk, t, r, a, b) IR_emit_instr_binary((bld), (blk), INSTR_ADD, (t), (r), (a), (b))
#define IR_emit_instr_sub(bld, blk, t, r, a, b) IR_emit_instr_binary((bld), (blk), INSTR_SUB, (t), (r), (a), (b))
#define IR_emit_instr_mul(bld, blk, t, r, a, b) IR_emit_instr_binary((bld), (blk), INSTR_MUL, (t), (r), (a), (b))
#define IR_emit_instr_and(bld, blk, t, r, a, b) IR_emit_instr_binary((bld), (blk), INSTR_AND, (t), (r), (a), (b))
#define IR_emit_instr_or(bld, blk, t, r, a, b) IR_emit_instr_binary((bld), (blk), INSTR_OR, (t), (r), (a), (b))
#define IR_emit_instr_xor(bld, blk, t, r, a, b) IR_emit_instr_binary((bld), (blk), INSTR_XOR, (t), (r), (a), (b))

static void IR_emit_instr_binary(IR_ProcBuilder* builder, BBlock* bblock, InstrKind kind, Type* type, IR_Reg r, IR_Reg a, IR_Reg b)
{
    Instr* instr = IR_new_instr(builder->arena, kind);
    instr->binary.type = type;
    instr->binary.r = r;
    instr->binary.a = a;
    instr->binary.b = b;

    IR_add_instr(builder, bblock, instr);
}

#define IR_emit_instr_sar(bld, blk, t, r, a, b) IR_emit_instr_shift((bld), (blk), INSTR_SAR, (t), (r), (a), (b))
#define IR_emit_instr_shl(bld, blk, t, r, a, b) IR_emit_instr_shift((bld), (blk), INSTR_SHL, (t), (r), (a), (b))
static void IR_emit_instr_shift(IR_ProcBuilder* builder, BBlock* bblock, InstrKind kind, Type* type, IR_Reg r, IR_Reg a, IR_Reg b)
{
    Instr* instr = IR_new_instr(builder->arena, kind);
    instr->shift.type = type;
    instr->shift.r = r;
    instr->shift.a = a;
    instr->shift.b = b;

    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_div(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, IR_Reg a, IR_Reg b)
{
    assert(type->kind == TYPE_INTEGER);

    InstrKind kind = type->as_integer.is_signed ? INSTR_SDIV : INSTR_UDIV;
    Instr* instr = IR_new_instr(builder->arena, kind);

    instr->binary.type = type;
    instr->binary.r = r;
    instr->binary.a = a;
    instr->binary.b = b;

    IR_add_instr(builder, bblock, instr);
}

#define IR_emit_instr_not(b, blk, t, r, a) IR_emit_instr_unary((b), (blk), INSTR_NOT, (t), (r), (a))
#define IR_emit_instr_neg(b, blk, t, r, a) IR_emit_instr_unary((b), (blk), INSTR_NEG, (t), (r), (a))

static void IR_emit_instr_unary(IR_ProcBuilder* builder, BBlock* bblock, InstrKind kind, Type* type, IR_Reg r, IR_Reg a)
{
    Instr* instr = IR_new_instr(builder->arena, kind);
    instr->unary.type = type;
    instr->unary.r = r;
    instr->unary.a = a;

    IR_add_instr(builder, bblock, instr);
}

#define IR_emit_instr_trunc(b, blk, dt, st, r, a) IR_emit_instr_convert((b), (blk), INSTR_TRUNC, (dt), (st), (r), (a)) 
#define IR_emit_instr_zext(b, blk, dt, st, r, a) IR_emit_instr_convert((b), (blk), INSTR_ZEXT, (dt), (st), (r), (a)) 
#define IR_emit_instr_sext(b, blk, dt, st, r, a) IR_emit_instr_convert((b), (blk), INSTR_SEXT, (dt), (st), (r), (a)) 

static void IR_emit_instr_convert(IR_ProcBuilder* builder, BBlock* bblock, InstrKind kind, Type* dst_type, Type* src_type,
                                  IR_Reg r, IR_Reg a)
{
    Instr* instr = IR_new_instr(builder->arena, kind);
    instr->convert.dst_type = dst_type;
    instr->convert.src_type = src_type;
    instr->convert.r = r;
    instr->convert.a = a;

    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_limm(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, Scalar imm)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_LIMM);
    instr->limm.type = type;
    instr->limm.r = r;
    instr->limm.imm = imm;

    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_load(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, MemAddr addr)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_LOAD);
    instr->load.type = type;
    instr->load.r = r;
    instr->load.addr = addr;
    
    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_laddr(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, MemAddr addr)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_LADDR);
    instr->laddr.type = type;
    instr->laddr.r = r;
    instr->laddr.addr = addr;
    
    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_store(IR_ProcBuilder* builder, BBlock* bblock, Type* type, MemAddr addr, IR_Reg a)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_STORE);
    instr->store.type = type;
    instr->store.addr = addr;
    instr->store.a = a;

    IR_add_instr(builder, bblock, instr);
}

static Instr* IR_emit_instr_cmp(IR_ProcBuilder* builder, BBlock* bblock, Type* type, ConditionKind cond, IR_Reg r, IR_Reg a, IR_Reg b)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_CMP);
    instr->cmp.type = type;
    instr->cmp.cond = cond;
    instr->cmp.r = r;
    instr->cmp.a = a;
    instr->cmp.b = b;

    IR_add_instr(builder, bblock, instr);

    return instr;
}

static Instr* IR_emit_instr_cond_jmp(IR_ProcBuilder* builder, BBlock* bblock, BBlock* true_bb, BBlock* false_bb, IR_Reg a)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_COND_JMP);
    instr->cond_jmp.from = bblock;
    instr->cond_jmp.true_bb = true_bb;
    instr->cond_jmp.false_bb = false_bb;
    instr->cond_jmp.a = a;

    IR_add_instr(builder, bblock, instr);

    return instr;
}

static Instr* IR_emit_instr_jmp(IR_ProcBuilder* builder, BBlock* bblock, BBlock* target)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_JMP);
    instr->jmp.from = bblock;
    instr->jmp.target = target;

    IR_add_instr(builder, bblock, instr);

    return instr;
}

static void IR_emit_instr_call(IR_ProcBuilder* builder, BBlock* bblock, Symbol* sym, IR_Reg r, u32 num_args, InstrCallArg* args)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_CALL);
    instr->call.sym = sym;
    instr->call.r = r;
    instr->call.num_args = num_args;
    instr->call.args = args;

    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_call_indirect(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg loc, IR_Reg r, u32 num_args, InstrCallArg* args)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_CALL_INDIRECT);
    instr->calli.proc_type = type;
    instr->calli.loc = loc;
    instr->calli.r = r;
    instr->calli.num_args = num_args;
    instr->calli.args = args;

    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_ret(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg a)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_RET);
    instr->ret.type = type;
    instr->ret.a = a;

    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_memcpy(IR_ProcBuilder* builder, BBlock* bblock, Type* type, MemAddr dst, MemAddr src)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_MEMCPY);
    instr->memcpy.type = type;
    instr->memcpy.dst = dst;
    instr->memcpy.src = src;

    IR_add_instr(builder, bblock, instr);
}

static void IR_emit_instr_phi(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, IR_Reg a, IR_Reg b)
{
    Instr* instr = IR_new_instr(builder->arena, INSTR_PHI);
    instr->phi.type = type;
    instr->phi.r = r;
    instr->phi.a = a;
    instr->phi.b = b;

    IR_add_instr(builder, bblock, instr);
}

static bool IR_type_fits_in_reg(Type* type)
{
    return type->size <= PTR_SIZE;
}

static IR_Reg IR_next_reg(IR_ProcBuilder* builder)
{
    Symbol* sym = builder->curr_proc;
    IR_Reg next_reg = sym->as_proc.num_regs++;

    return next_reg;
}

static MemAddr IR_sym_as_addr(Symbol* sym)
{
    MemAddr addr = {.base_kind = MEM_BASE_SYM, .base.sym = sym, .index_reg = IR_REG_COUNT};
    return addr;
}

static MemAddr IR_strlit_as_addr(StrLit* str_lit)
{
    MemAddr addr = {.base_kind = MEM_BASE_STR_LIT, .base.str_lit = str_lit, .index_reg = IR_REG_COUNT};
    return addr;
}

static void IR_get_object_addr(IR_ProcBuilder* builder, BBlock* bblock, MemAddr* dst, IR_Operand* src)
{
    Type* src_type = src->type;

    if (src->kind == IR_OPERAND_VAR) {
        if (src->sym->is_local) {
            dst->base_kind = MEM_BASE_SYM;
            dst->base.sym = src->sym;
        }
        else {
            IR_Reg dst_reg = IR_next_reg(builder);
            IR_emit_instr_laddr(builder, bblock, src_type, dst_reg, IR_sym_as_addr(src->sym));

            dst->base_kind = MEM_BASE_REG;
            dst->base.reg = dst_reg;
        }

        dst->index_reg = IR_REG_COUNT;
        dst->disp = 0;
        dst->scale = 0;
    }
    else if (src->kind == IR_OPERAND_DEREF_ADDR) {
        *dst = src->addr;
    }
    else {
        assert(src->kind == IR_OPERAND_STR_LIT);

        IR_Reg dst_reg = IR_next_reg(builder);
        IR_emit_instr_laddr(builder, bblock, src_type, dst_reg, IR_strlit_as_addr(src->str_lit));

        dst->base_kind = MEM_BASE_REG;
        dst->base.reg = dst_reg;
        dst->index_reg = IR_REG_COUNT;
        dst->disp = 0;
        dst->scale = 0;
    }
}

static void IR_operand_from_sym(IR_Operand* op, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR || sym->kind == SYMBOL_PROC);
    op->kind = (sym->kind == SYMBOL_VAR) ? IR_OPERAND_VAR : IR_OPERAND_PROC;
    op->type = sym->type;
    op->sym = sym;
}

static void IR_new_deferred_sc_jmp(IR_ProcBuilder* builder, IR_DeferredCmp* cmp, Instr* cmp_instr, bool result, Instr* jmp_instr)
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
    new_node->cmp = cmp_instr;
    new_node->result = result;
    new_node->jmp = jmp_instr;
}

static void IR_del_deferred_sc_jmp(IR_ProcBuilder* builder, IR_DeferredCmp* cmp, IR_DeferredJmpcc* prev_jmp, IR_DeferredJmpcc* jmp)
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

static void IR_fix_sc_jmp_path(IR_DeferredJmpcc* def_jmp, bool desired_result)
{
    if (def_jmp->result != desired_result) {
        def_jmp->cmp->cmp.cond = ir_opposite_cond[def_jmp->cmp->cmp.cond];
        def_jmp->result = desired_result;
    }
}

static void IR_copy_sc_jmp(IR_ProcBuilder* builder, BBlock* bblock, IR_DeferredJmpcc* dst_jmp, IR_DeferredJmpcc* src_jmp,
                           bool desired_result)
{
    *dst_jmp = *src_jmp;

    IR_fix_sc_jmp_path(dst_jmp, desired_result);

    if (!dst_jmp->jmp) {
        dst_jmp->jmp = IR_emit_instr_cond_jmp(builder, bblock, NULL, NULL, dst_jmp->cmp->cmp.r);
    }
}

static BBlock* IR_execute_deferred_cmp(IR_ProcBuilder* builder, BBlock* bblock, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEFERRED_CMP);

    BBlock* e_bblock;
    IR_DeferredCmp* def_cmp = &operand->cmp;
    IR_Reg dst_reg;

    bool has_sc_jmps = def_cmp->first_sc_jmp != NULL;
    bool has_final_jmp = def_cmp->final_jmp.jmp != NULL;

    if (!has_sc_jmps && !has_final_jmp) {
        e_bblock = bblock;
        dst_reg = IR_next_reg(builder);

        IR_emit_instr_zext(builder, bblock, operand->type, builtin_types[BUILTIN_TYPE_U8].type, dst_reg,
                           def_cmp->final_jmp.cmp->cmp.cond);
    }
    else {
        BBlock* t_bblock = IR_alloc_bblock(builder);
        BBlock* f_bblock = IR_alloc_bblock(builder);
        e_bblock = IR_alloc_bblock(builder);

        IR_connect_bblocks(bblock, t_bblock);
        IR_connect_bblocks(bblock, f_bblock);
        IR_connect_bblocks(t_bblock, e_bblock);
        IR_connect_bblocks(f_bblock, e_bblock);

        //
        // True control path.
        //

        // Patch short-circuit jumps that jump to the "true" control path.
        for (IR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next) {
            if (it->result)
                IR_patch_jmp_target(it->jmp, t_bblock);
        }

        // Move the literal 1 into destination register.
        IR_Reg one_reg = IR_next_reg(builder);
        IR_emit_instr_limm(builder, t_bblock, operand->type, one_reg, ir_one_imm);

        // Create a jump to skip the false control path.
        Instr* jmp_skip_false = IR_emit_instr_jmp(builder, t_bblock, e_bblock);

        //
        // False control path.
        //

        // Patch short-circuit jumps that jump to the "false" control path.
        for (IR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next) {
            if (!it->result)
                IR_patch_jmp_target(it->jmp, f_bblock);
        }

        // Patch final jmp so that it jumps to "false" control path.
        IR_DeferredJmpcc* final_jmp = &def_cmp->final_jmp;
        IR_fix_sc_jmp_path(final_jmp, false);
        IR_patch_jmp_target(final_jmp->jmp, f_bblock);

        // This is the "false" control path. Move the literal 0 into destination register.
        IR_Reg zero_reg = IR_next_reg(builder);
        IR_emit_instr_limm(builder, f_bblock, operand->type, zero_reg, ir_zero_imm);
        IR_emit_instr_jmp(builder, f_bblock, e_bblock); // NOTE: Not needed in actual assembly (fall-through)

        //
        // End block.
        //

        // Emit PHI instruction.
        dst_reg = IR_next_reg(builder);
        IR_emit_instr_phi(builder, e_bblock, operand->type, dst_reg, one_reg, zero_reg);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;

    return e_bblock;
}

static void IR_execute_deref(IR_ProcBuilder* builder, BBlock* bblock, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEREF_ADDR);

    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_load(builder, bblock, operand->type, dst_reg, operand->addr);

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_execute_lea(IR_ProcBuilder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_MEM_ADDR);

    // The operand currently holds a memory address.
    // This function executes the "load-effective-address" call.
    MemAddr addr = operand->addr;
    IR_Reg base_reg = addr.base_kind == MEM_BASE_REG ? addr.base.reg : IR_REG_COUNT;
    IR_Reg index_reg = addr.scale ? addr.index_reg : IR_REG_COUNT;

    bool has_base_reg = base_reg < IR_REG_COUNT;
    bool has_index_reg = index_reg < IR_REG_COUNT;
    bool has_disp = addr.disp != 0;

    IR_Reg dst_reg;

    if (has_base_reg && !has_index_reg && !has_disp) {
        // No need to emit any instructions. Just keep address in base register.
        dst_reg = base_reg;
    }
    else {
        dst_reg = IR_next_reg(builder);
        IR_emit_instr_laddr(builder, operand->type, dst_reg, addr);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_ptr_to_mem_op(IR_ProcBuilder* builder, IR_Operand* operand)
{
    assert(operand->type->kind == TYPE_PTR);

    if (operand->kind == IR_OPERAND_MEM_ADDR) {
        return;
    }

    IR_Reg base_reg = IR_REG_COUNT;

    if (operand->kind == IR_OPERAND_VAR) {
        base_reg = IR_next_reg(builder);
        IR_emit_instr_load(builder, operand->type, base_reg, IR_sym_as_addr(operand->sym));

    }
    else if (operand->kind == IR_OPERAND_DEREF_ADDR) {
        // Occurs when dereferencing a pointer to a pointer.
        // Ex:
        //     var p  : ^char = "hi";
        //     var pp : ^^char = ^p;
        //     #writeout(*pp + 1, 1); // Happens for expression `*pp + 1`
        IR_execute_deref(builder, operand);
        base_reg = operand->reg;
    }

    assert(base_reg != IR_REG_COUNT);

    operand->kind = IR_OPERAND_MEM_ADDR;
    operand->addr.base_kind = MEM_BASE_REG;
    operand->addr.base.reg = base_reg;
    operand->addr.index_reg = IR_REG_COUNT;
    operand->addr.scale = 0;
    operand->addr.disp = 0;
}

static void IR_op_to_r(IR_ProcBuilder* builder, IR_Operand* operand)
{
    if (operand->kind == IR_OPERAND_REG) {
        return;
    }

    switch (operand->kind) {
    case IR_OPERAND_MEM_ADDR:
        IR_execute_lea(builder, operand);
        break;
    case IR_OPERAND_DEREF_ADDR:
        IR_execute_deref(builder, operand);
        break;
    case IR_OPERAND_DEFERRED_CMP:
        IR_execute_deferred_cmp(builder, operand);
        break;
    case IR_OPERAND_IMM: {
        IR_Reg reg = IR_next_reg(builder);

        IR_emit_instr_limm(builder, operand->type, reg, operand->imm);

        operand->kind = IR_OPERAND_REG;
        operand->reg = reg;
        break;
    }
    case IR_OPERAND_PROC: {
        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_laddr(builder, operand->type, reg, IR_sym_as_addr(operand->sym));

        operand->kind = IR_OPERAND_REG;
        operand->reg = reg;
        break;
    }
    default: {
        assert(operand->kind == IR_OPERAND_VAR);
        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_load(builder, operand->type, reg, IR_sym_as_addr(operand->sym));

        operand->kind = IR_OPERAND_REG;
        operand->reg = reg;
        break;
    }
    }
}

static void IR_emit_assign(IR_ProcBuilder* builder, IR_Operand* lhs, IR_Operand* rhs);

// Emit code for initializing an array with an initializer.
//    var a: [11] int = {0, 1, 2, 3};
static void IR_emit_array_init(IR_ProcBuilder* builder, IR_Operand* array_op, IR_Operand* init_op)
{
    assert(array_op->kind == IR_OPERAND_VAR || array_op->kind == IR_OPERAND_DEREF_ADDR);
    assert(init_op->kind == IR_OPERAND_ARRAY_INIT);
    assert(array_op->type->kind == TYPE_ARRAY);

    Type* arr_type = array_op->type;
    Type* ptr_type = try_array_decay(builder->arena, &builder->type_cache->ptrs, arr_type);
    Type* elem_type = ptr_type->as_ptr.base;

    // Decay array into pointer to the first elem.
    IR_Operand base_ptr_op = {.kind = IR_OPERAND_MEM_ADDR, .type = ptr_type};
    IR_get_object_addr(builder, &base_ptr_op.addr, array_op);

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
static void IR_emit_array_str_init(IR_ProcBuilder* builder, IR_Operand* array_op, IR_Operand* init_op)
{
    assert(array_op->kind == IR_OPERAND_VAR || array_op->kind == IR_OPERAND_DEREF_ADDR);
    assert(init_op->kind == IR_OPERAND_STR_LIT);
    assert(array_op->type->kind == TYPE_ARRAY);

    Type* arr_type = array_op->type;
    Type* ptr_type = try_array_decay(builder->arena, &builder->type_cache->ptrs, arr_type);
    Type* elem_type = ptr_type->as_ptr.base;
    u64 num_elems = arr_type->as_array.len;

    StrLit* str_lit = init_op->str_lit;
    const char* str = str_lit->str;

    assert((str_lit->len + 1) == num_elems);

    // Decay array into pointer to the first elem.
    IR_Operand base_ptr_op = {.kind = IR_OPERAND_MEM_ADDR, .type = ptr_type};
    IR_get_object_addr(builder, &base_ptr_op.addr, array_op);

    for (u64 elem_index = 0; elem_index < num_elems; elem_index += 1) {
        IR_Operand char_op = {.kind = IR_OPERAND_IMM, .type = elem_type, .imm.as_int._u64 = str[elem_index]};

        IR_Operand elem_ptr_op = {.kind = IR_OPERAND_DEREF_ADDR, .type = elem_type, .addr = base_ptr_op.addr};
        elem_ptr_op.addr.disp += elem_type->size * elem_index;

        IR_emit_assign(builder, &elem_ptr_op, &char_op);
    }

    // TODO: Reduce the number of assignment (mov) instructions by initializing
    // multiple elements at a time (one machine word's worth).
}

static void IR_emit_assign(IR_ProcBuilder* builder, IR_Operand* lhs, IR_Operand* rhs)
{
    MemAddr dst_addr;
    IR_get_object_addr(builder, &dst_addr, lhs);

    if (rhs->kind == IR_OPERAND_IMM) {
        IR_Reg r = IR_next_reg(builder);

        IR_emit_instr_limm(builder, rhs->type, r, rhs->imm);
        IR_emit_instr_store(builder, lhs->type, dst_addr, r);
    }
    else if (rhs->kind == IR_OPERAND_ARRAY_INIT) {
        IR_emit_array_init(builder, lhs, rhs);
    }
    else if (rhs->kind == IR_OPERAND_STR_LIT) {
        IR_emit_array_str_init(builder, lhs, rhs);
    }
    else if (IR_type_fits_in_reg(rhs->type)) {
        IR_op_to_r(builder, rhs);
        IR_emit_instr_store(builder, lhs->type, dst_addr, rhs->reg);
    }
    else {
        MemAddr src_addr;
        IR_get_object_addr(builder, &src_addr, rhs);
        IR_emit_instr_memcpy(builder, lhs->type, dst_addr, src_addr);
    }
}

static void IR_push_scope(IR_ProcBuilder* builder, Scope* scope)
{
    builder->curr_scope = scope;
}

static void IR_pop_scope(IR_ProcBuilder* builder)
{
    builder->curr_scope = builder->curr_scope->parent;
}

//////////////////////////////////////////////////////
//
//         Walk AST and emit IR instructions
//
//////////////////////////////////////////////////////

static void IR_emit_stmt(IR_ProcBuilder* builder, Stmt* stmt, u32* break_target, u32* continue_target);
static void IR_emit_expr(IR_ProcBuilder* builder, Expr* expr, IR_Operand* dst);

static void IR_emit_expr_ident(IR_ProcBuilder* builder, ExprIdent* eident, IR_Operand* dst)
{
    Symbol* sym = NULL;

    if (eident->mod_ns) {
        Symbol* sym_modns = lookup_symbol(builder->curr_scope, eident->mod_ns);
        StmtImport* stmt = (StmtImport*)sym_modns->as_mod.stmt;
        Identifier* sym_name = get_import_sym_name(stmt, eident->name);

        sym = module_get_export_sym(sym_modns->as_mod.mod, sym_name);
    }
    else {
        sym = lookup_symbol(builder->curr_scope, eident->name);
    }

    assert(sym);

    IR_operand_from_sym(dst, sym);
}

static void IR_emit_ptr_int_add(IR_ProcBuilder* builder, IR_Operand* dst, IR_Operand* ptr_op, IR_Operand* int_op, bool add)
{
    u64 base_size = ptr_op->type->as_ptr.base->size;

    IR_ptr_to_mem_op(builder, ptr_op);

    if (int_op->kind == IR_OPERAND_IMM) {
        if (add)
            ptr_op->addr.disp += base_size * int_op->imm.as_int._u64;
        else
            ptr_op->addr.disp -= base_size * int_op->imm.as_int._u64;
    }
    else {
        if (ptr_op->addr.scale) {
            IR_op_to_r(builder, int_op);

            IR_Reg r = IR_next_reg(builder);
            IR_Reg a = ptr_op->addr.index_reg;
            IR_Reg b = int_op->reg;

            if (add)
                IR_emit_instr_add(builder, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);
            else
                IR_emit_instr_sub(builder, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);

            ptr_op->addr.index_reg = r;
        }
        else {
            IR_op_to_r(builder, int_op);

            if (!add) {
                IR_Reg a = int_op->reg;

                int_op->reg = IR_next_reg(builder);
                IR_emit_instr_neg(builder, int_op->type, int_op->reg, a);
            }

            ptr_op->addr.scale = base_size;
            ptr_op->addr.index_reg = int_op->reg;
        }
    }

    *dst = *ptr_op;
}

static void IR_emit_binary_cmp(IR_ProcBuilder* builder, ConditionKind cond_kind, Type* dst_type, IR_Operand* dst_op,
                                IR_Operand* left_op, IR_Operand* right_op)
{
    assert(left_op->type == right_op->type);
    IR_op_to_r(builder, left_op);
    IR_op_to_r(builder, right_op);

    IR_Reg cmp_reg = IR_next_reg(builder);
    Instr* cmp_instr = IR_emit_instr_cmp(builder, left_op->type, cond_kind, cmp_reg, left_op->reg, right_op->reg);

    dst_op->type = dst_type;
    dst_op->kind = IR_OPERAND_DEFERRED_CMP;
    dst_op->cmp.final_jmp.cmp = cmp_instr;
    dst_op->cmp.final_jmp.result = true;
    dst_op->cmp.final_jmp.jmp = NULL;
    dst_op->cmp.first_sc_jmp = NULL;
    dst_op->cmp.last_sc_jmp = NULL;
}

static void IR_emit_short_circuit_cmp(IR_ProcBuilder* builder, IR_Operand* dst_op, ExprBinary* expr)
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
    ConditionKind short_circuit_cond;

    if (expr->op == TKN_LOGIC_AND) {
        short_circuit_val = false;
        short_circuit_cond = COND_EQ;
    }
    else {
        assert(expr->op == TKN_LOGIC_OR);
        short_circuit_val = true;
        short_circuit_cond = COND_NEQ;
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
        // TODO: Provide from block
        IR_copy_sc_jmp(builder, &j, &left_op.cmp.final_jmp, short_circuit_val);
        IR_new_deferred_sc_jmp(builder, &dst_op->cmp, j.cmp, j.result, j.jmp);
    }

    // The left subexpression is some computation (not a deferred comparison). Compare the left subexpression to zero
    // and create a short-circuit jmp.
    else {
        IR_op_to_r(builder, &left_op);

        IR_Reg imm_reg = IR_next_reg(builder);
        IR_emit_instr_limm(builder, left_op.type, imm_reg, ir_zero_imm);

        IR_Reg cmp_reg = IR_next_reg(builder);
        Instr* cmp_instr = IR_emit_instr_cmp(builder, left_op.type, short_circuit_cond, cmp_reg, left_op.reg, imm_reg);
        Instr* jmp_instr = IR_emit_instr_cond_jmp(builder, IR_alloc_jmp_target(builder, 0), cmp_reg);

        IR_new_deferred_sc_jmp(builder, &dst_op->cmp, cmp_instr, short_circuit_val, jmp_instr);
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
        // TODO: Provide from block
        IR_copy_sc_jmp(builder, &dst_op->cmp.final_jmp, &right_op.cmp.final_jmp, false);
    }
    // The right subexpression is some computation (not a deferred comparison). Compare the right subexpression to zero
    // and create a final jump.
    else {
        IR_op_to_r(builder, &right_op);

        IR_Reg imm_reg = IR_next_reg(builder);
        IR_emit_instr_limm(builder, right_op.type, imm_reg, ir_zero_imm);

        IR_Reg cmp_reg = IR_next_reg(builder);
        Instr* cmp_instr = IR_emit_instr_cmp(builder, right_op.type, COND_EQ, cmp_reg, right_op.reg, imm_reg);
        Instr* jmp_instr = IR_emit_instr_cond_jmp(builder, IR_alloc_jmp_target(builder, 0), cmp_reg);

        dst_op->cmp.final_jmp.result = false;
        dst_op->cmp.final_jmp.jmp = jmp_instr;
        dst_op->cmp.final_jmp.cmp = cmp_instr;
    }
}

static void IR_emit_expr_binary(IR_ProcBuilder* builder, ExprBinary* expr, IR_Operand* dst)
{
    if (expr->op == TKN_LOGIC_AND || expr->op == TKN_LOGIC_OR) {
        IR_emit_short_circuit_cmp(builder, dst, expr);
        return;
    }

    Type* result_type = expr->super.type;
    IR_Operand left = {0};
    IR_Operand right = {0};

    IR_emit_expr(builder, expr->left, &left);
    IR_emit_expr(builder, expr->right, &right);

    switch (expr->op) {
    case TKN_PLUS: {
        bool left_is_ptr = left.type->kind == TYPE_PTR;
        bool right_is_ptr = right.type->kind == TYPE_PTR;

        if (left_is_ptr) {
            IR_emit_ptr_int_add(builder, dst, &left, &right, true);
        }
        else if (right_is_ptr) {
            IR_emit_ptr_int_add(builder, dst, &right, &left, true);
        }
        else {
            IR_op_to_r(builder, &left);
            IR_op_to_r(builder, &right);

            IR_Reg dst_reg = IR_next_reg(builder);

            IR_emit_instr_add(builder, result_type, dst_reg, left.reg, right.reg);

            dst->kind = IR_OPERAND_REG;
            dst->type = result_type;
            dst->reg = dst_reg;
        }
        break;
    }
    case TKN_MINUS: {
        bool left_is_ptr = left.type->kind == TYPE_PTR;
        bool right_is_ptr = right.type->kind == TYPE_PTR;

        // ptr - int => ptr
        if (left_is_ptr && !right_is_ptr) {
            IR_emit_ptr_int_add(builder, dst, &left, &right, false);
        }
        // ptr - ptr => s64
        else if (left_is_ptr && right_is_ptr) {
            u64 base_size = left.type->as_ptr.base->size;
            u32 base_size_log2 = (u32)clp2(base_size);

            IR_op_to_r(builder, &left);
            IR_op_to_r(builder, &right);
            IR_Reg dst_reg = IR_next_reg(builder);

            IR_emit_instr_sub(builder, result_type, dst_reg, left.reg, right.reg);

            if (base_size_log2 > 0) {

                // Load shift amount into a register.
                Scalar shift_arg = {.as_int._u32 = base_size_log2};
                IR_Reg shift_reg = IR_next_reg(builder);
                IR_emit_instr_limm(builder, builtin_types[BUILTIN_TYPE_U8].type, shift_reg, shift_arg);

                // Shift result of subtraction by the shift amount.
                IR_Reg tmp_reg = dst_reg;
                dst_reg = IR_next_reg(builder);
                IR_emit_instr_sar(builder, result_type, dst_reg, tmp_reg, shift_reg);
            }

            dst->kind = IR_OPERAND_REG;
            dst->type = result_type;
            dst->reg = dst_reg;
        }
        // int - int => int
        else {
            IR_op_to_r(builder, &left);
            IR_op_to_r(builder, &right);
            IR_Reg dst_reg = IR_next_reg(builder);
            IR_emit_instr_sub(builder, result_type, dst_reg, left.reg, right.reg);

            dst->kind = IR_OPERAND_REG;
            dst->type = result_type;
            dst->reg = dst_reg;
        }
        break;
    }
    case TKN_ASTERISK: {
        IR_op_to_r(builder, &left);
        IR_op_to_r(builder, &right);
        IR_Reg dst_reg = IR_next_reg(builder);

        // TODO: Emit a shift instruction if one of the operands is a power-of-two immediate.
        IR_emit_instr_mul(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_DIV: {
        IR_op_to_r(builder, &left);
        IR_op_to_r(builder, &right);
        IR_Reg dst_reg = IR_next_reg(builder);

        // TODO: Emit a shift instruction if the second operand is a power-of-two immediate.
        IR_emit_instr_div(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_RSHIFT: {
        IR_op_to_r(builder, &left);
        IR_op_to_r(builder, &right);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_sar(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_LSHIFT: {
        IR_op_to_r(builder, &left);
        IR_op_to_r(builder, &right);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_shl(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_AND: {
        IR_op_to_r(builder, &left);
        IR_op_to_r(builder, &right);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_and(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_OR: {
        IR_op_to_r(builder, &left);
        IR_op_to_r(builder, &right);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_or(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_CARET: {
        IR_op_to_r(builder, &left);
        IR_op_to_r(builder, &right);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_xor(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_EQ: {
        IR_emit_binary_cmp(builder, COND_EQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_NOTEQ: {
        IR_emit_binary_cmp(builder, COND_NEQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_LT: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_LT : COND_U_LT;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_LTEQ: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_LTEQ : COND_U_LTEQ;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GT: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_GT : COND_U_GT;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GTEQ: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_GTEQ : COND_U_GTEQ;

        IR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void IR_emit_expr_unary(IR_ProcBuilder* builder, ExprUnary* expr, IR_Operand* dst)
{
    Type* result_type = expr->super.type;

    switch (expr->op) {
    case TKN_PLUS: {
        IR_emit_expr(builder, expr->expr, dst);
        break;
    }
    case TKN_MINUS: // Two's compliment negation.
    {
        IR_Operand src;

        IR_emit_expr(builder, expr->expr, &src);
        IR_op_to_r(builder, &src);

        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_neg(builder, result_type, dst_reg, src.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_NEG: // Bitwise not
    {
        IR_Operand src;

        IR_emit_expr(builder, expr->expr, &src);
        IR_op_to_r(builder, &src);

        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_not(builder, result_type, dst_reg, src.reg);

        dst->kind = IR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
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
            IR_op_to_r(builder, &inner_op);

            IR_Reg imm_reg = IR_next_reg(builder);
            IR_emit_instr_limm(builder, inner_op.type, imm_reg, ir_zero_imm);

            IR_Reg dst_reg = IR_next_reg(builder);
            Instr* cmp_instr = IR_emit_instr_cmp(builder, inner_op.type, COND_EQ, dst_reg, inner_op.reg, imm_reg);

            dst->cmp.final_jmp.cmp = cmp_instr;
            dst->cmp.final_jmp.result = true;
            dst->cmp.first_sc_jmp = NULL;
            dst->cmp.last_sc_jmp = NULL;
            dst->cmp.final_jmp.jmp = NULL;
        }

        break;
    }
    case TKN_ASTERISK: {
        IR_emit_expr(builder, expr->expr, dst);
        IR_ptr_to_mem_op(builder, dst);

        dst->kind = IR_OPERAND_DEREF_ADDR;
        dst->type = result_type;
        break;
    }
    case TKN_CARET: // Address-of operator
    {
        IR_Operand src;
        IR_emit_expr(builder, expr->expr, &src);

        dst->kind = IR_OPERAND_MEM_ADDR;
        dst->type = result_type;

        if (src.kind == IR_OPERAND_DEREF_ADDR) {
            dst->addr = src.addr;
        }
        else {
            assert(src.kind == IR_OPERAND_VAR);

            IR_Reg dst_reg = IR_next_reg(builder);

            IR_emit_instr_laddr(builder, src.type, dst_reg, IR_sym_as_addr(src.sym));

            dst->addr.base_kind = MEM_BASE_REG;
            dst->addr.base.reg = dst_reg;
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

static void IR_emit_expr_field(IR_ProcBuilder* builder, ExprField* expr_field, IR_Operand* dst)
{
    IR_Operand obj_op = {0};
    IR_emit_expr(builder, expr_field->object, &obj_op);

    Type* obj_type;
    MemAddr obj_addr;

    if (obj_op.type->kind == TYPE_PTR) {
        //
        // This pointer points to the actual object.
        //

        obj_type = obj_op.type->as_ptr.base;
        IR_ptr_to_mem_op(builder, &obj_op);
        obj_addr = obj_op.addr;
    }
    else {
        //
        // Accessing the object field directly.
        //

        obj_type = obj_op.type;
        IR_get_object_addr(builder, &obj_addr, &obj_op);
    }

    size_t field_offset = get_type_aggregate_field(obj_type, expr_field->field)->offset;

    dst->kind = IR_OPERAND_DEREF_ADDR;
    dst->type = expr_field->super.type;
    dst->addr = obj_addr;

    // Add in the field's byte offset from the beginning of the object.
    dst->addr.disp += (u32)field_offset;
}

static void IR_emit_expr_index(IR_ProcBuilder* builder, ExprIndex* expr_index, IR_Operand* dst)
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

static void IR_emit_int_cast(IR_ProcBuilder* builder, IR_Operand* src_op, IR_Operand* dst_op)
{
    // NOTE:
    // This function treats pointers like integers. The IR currently implements "opaque" pointers, so
    // there are no explicit instructions for converting from one ptr type to another, or converting to/from int/ptr.
    assert(src_op->kind != IR_OPERAND_IMM); // Should be prevented by resolver.

    // We need the src expression to be in a register.
    IR_op_to_r(builder, src_op);

    IR_Reg dst_reg = IR_REG_COUNT;
    IR_Reg src_reg = src_op->reg;

    Type* src_type = src_op->type;
    Type* dst_type = dst_op->type;
    size_t src_size = src_type->size;
    size_t dst_size = dst_type->size;

    // Integers are the same size. This is a NO-OP even if any of the types is a ptr type.
    if (src_size == dst_size) {
        dst_reg = src_reg;
    }
    // Truncate from larger type to smaller type.
    else if (src_size > dst_size) {
        dst_reg = IR_next_reg(builder);
        IR_emit_instr_trunc(builder, dst_type, src_type, dst_reg, src_reg);
    }
    // Extend (sign or zero) src to larger type.
    else {
        dst_reg = IR_next_reg(builder);
        InstrKind instr_kind = (src_type->kind == TYPE_INTEGER) && src_type->as_integer.is_signed ? INSTR_SEXT : INSTR_ZEXT;
        IR_emit_instr_convert(builder, instr_kind, dst_type, src_type, dst_reg, src_reg);
    }

    if (dst_op->type->kind == TYPE_PTR) {
        dst_op->kind = IR_OPERAND_MEM_ADDR;
        dst_op->addr.base_kind = MEM_BASE_REG;
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

static void IR_emit_expr_cast(IR_ProcBuilder* builder, ExprCast* expr_cast, IR_Operand* dst_op)
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
        dst_op->kind = IR_OPERAND_MEM_ADDR;

        IR_get_object_addr(builder, &dst_op->addr, &src_op);
    }
    else {
        IR_emit_int_cast(builder, &src_op, dst_op);
    }
}

static void IR_setup_call_ret(IR_ProcBuilder* builder, ExprCall* expr_call, IR_Operand* dst_op)
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

static InstrCallArg* IR_setup_call_args(IR_ProcBuilder* builder, ExprCall* expr_call)
{
    u32 num_args = (u32)expr_call->num_args;
    InstrCallArg* args = alloc_array(builder->arena, InstrCallArg, num_args, false);

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
            IR_op_to_r(builder, &arg_op);

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

static void IR_emit_expr_call(IR_ProcBuilder* builder, ExprCall* expr_call, IR_Operand* dst_op)
{
    u32 num_args = (u32)expr_call->num_args;
    InstrCallArg* args = IR_setup_call_args(builder, expr_call);

    // Emit instructions for the procedure pointer/name.
    IR_Operand proc_op = {0};
    IR_emit_expr(builder, expr_call->proc, &proc_op);

    // Allocate register for return value, emit call instruction, and then cleanup.
    if (proc_op.kind == IR_OPERAND_PROC) {
        // Direct procedure call.
        IR_setup_call_ret(builder, expr_call, dst_op);
        IR_emit_instr_call(builder, proc_op.sym, dst_op->reg, num_args, args);
    }
    else {
        // Indirect procedure call through register.
        IR_op_to_r(builder, &proc_op);
        IR_setup_call_ret(builder, expr_call, dst_op);
        IR_emit_instr_call_indirect(builder, proc_op.type, proc_op.reg, dst_op->reg, num_args, args);
    }

    // Mark current procedure as non-leaf.
    builder->curr_proc->as_proc.is_nonleaf = true;
}

static void IR_emit_expr_compound_lit(IR_ProcBuilder* builder, ExprCompoundLit* expr, IR_Operand* dst)
{
    // TODO: Currently only support array initializers.
    assert(expr->super.type->kind == TYPE_ARRAY);
    assert(!expr->typespec);

    u64 initzer_index = 0;
    IR_ArrayMemberInitializer* ir_initzers = alloc_array(builder->tmp_arena, IR_ArrayMemberInitializer, expr->num_initzers, true);

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

static void IR_emit_expr(IR_ProcBuilder* builder, Expr* expr, IR_Operand* dst)
{
    if (expr->is_constexpr && expr->is_imm) {
        assert(type_is_scalar(expr->type));
        dst->kind = IR_OPERAND_IMM;
        dst->type = expr->type;
        dst->imm = expr->imm;

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
    case CST_ExprField:
        IR_emit_expr_field(builder, (ExprField*)expr, dst);
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


static void IR_emit_stmt_block_body(IR_ProcBuilder* builder, List* stmts, u32* break_target, u32* continue_target)
{
    for (List* it = stmts->next; it != stmts; it = it->next) {
        Stmt* s = list_entry(it, Stmt, lnode);
        IR_emit_stmt(builder, s, break_target, continue_target);
    }
}

static void IR_emit_stmt_block(IR_ProcBuilder* builder, StmtBlock* sblock, u32* break_target, u32* continue_target)
{
    IR_push_scope(builder, sblock->scope);
    IR_emit_stmt_block_body(builder, &sblock->stmts, break_target, continue_target);
    IR_pop_scope(builder);
}

static void IR_emit_stmt_return(IR_ProcBuilder* builder, StmtReturn* sret)
{
    IR_Operand expr_op = {0};
    IR_emit_expr(builder, sret->expr, &expr_op);
    IR_op_to_r(builder, &expr_op);

    IR_emit_instr_ret(builder, expr_op.type, expr_op.reg);
}

static void IR_emit_stmt_decl(IR_ProcBuilder* builder, StmtDecl* sdecl)
{
    if (sdecl->decl->kind == CST_DeclConst) {
        return;
    }

    assert(sdecl->decl->kind == CST_DeclVar);

    DeclVar* dvar = (DeclVar*)sdecl->decl;

    if (dvar->init) {
        IR_Operand rhs_op = {0};
        IR_Operand lhs_op = {0};

        IR_emit_expr(builder, dvar->init, &rhs_op);
        IR_operand_from_sym(&lhs_op, lookup_symbol(builder->curr_scope, dvar->super.name));

        IR_emit_assign(builder, &lhs_op, &rhs_op);
    }
}

static void IR_emit_stmt_expr(IR_ProcBuilder* builder, StmtExpr* sexpr)
{
    IR_Operand expr_op = {0};
    IR_emit_expr(builder, sexpr->expr, &expr_op);

    // Actually execute any deferred operations.
    switch (expr_op.kind) {
    case IR_OPERAND_DEREF_ADDR:
        IR_execute_deref(builder, &expr_op);
        break;
    case IR_OPERAND_DEFERRED_CMP:
        IR_execute_deferred_cmp(builder, &expr_op);
        break;
    case IR_OPERAND_MEM_ADDR:
        IR_execute_lea(builder, &expr_op);
        break;
    default:
        break;
    }
}

static void IR_emit_stmt_expr_assign(IR_ProcBuilder* builder, StmtExprAssign* stmt)
{
    switch (stmt->op_assign) {
    case TKN_ASSIGN: {
        IR_Operand lhs_op = {0};
        IR_Operand rhs_op = {0};

        IR_emit_expr(builder, stmt->left, &lhs_op);
        IR_emit_expr(builder, stmt->right, &rhs_op);

        IR_emit_assign(builder, &lhs_op, &rhs_op);
        break;
    }
    default:
        assert(!"Unsupported assignment operator in IR generation");
        break;
    }
}

static BBlock* IR_emit_stmt_if(IR_ProcBuilder* builder, BBlock* bblock, StmtIf* stmt, BBlock* break_target, BBlock* continue_target)
{
    Expr* cond_expr = stmt->if_blk.cond;
    Stmt* if_body = stmt->if_blk.body;
    Stmt* else_body = stmt->else_blk.body;

    // If expr is a compile-time constant, do not generate the unneeded branch!!
    if (cond_expr->is_constexpr && cond_expr->is_imm) {
        assert(type_is_scalar(cond_expr->type));
        bool cond_val = cond_expr->imm.as_int._u64 != 0;
        Stmt* body = cond_expr->imm.as_int._u64 != 0 ? if_body : else_body;

        return IR_emit_stmt(builder, bblock, body, break_target, continue_target);
    }

    IR_Operand cond_op = {0};
    BBlock* hdr_bb = IR_emit_expr(builder, bblock, cond_expr, &cond_op);

    BBlock* true_bb = IR_alloc_bblock(builder);
    BBlock* false_bb = NULL;
    BBlock* end_bb = IR_alloc_bblock(builder);
    BBlock* false_tgt = end_bb;

    IR_connect_bblocks(h_bblock, t_bblock);

    if (else_body) {
        false_tgt = false_bb = IR_alloc_bblock(builder);
        IR_connect_bblocks(hdr_bb, false_bb);
    }

    // If the condition is a chain of deferred comparisons, patch the jump targets
    if (cond_op.kind == IR_OPERAND_DEFERRED_CMP) {
        IR_DeferredJmpcc* final_jmp = &cond_op.cmp.final_jmp;

        // Create a jump to the "false" path if it doesn't yet exist.
        if (!final_jmp->jmp) {
            final_jmp->jmp = IR_emit_instr_cond_jmp(builder, hdr_bb, false_tgt, true_bb, cond_op.cmp.final_jmp.cmp->cmp.r);
        }
        else {
            IR_patch_jmp_target(final_jmp->jmp, false_tgt);
        }

        IR_fix_sc_jmp_path(final_jmp, false);

        // Patch the jump target for all short-circuit jmps that jump to the "true" control path.
        for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next) {
            IR_patch_jmp_target(it->jmp, (it->result ? true_bb : false_tgt));
        }

    }
    // If the condition is some computation, compare the condition to zero and create a conditional
    // jump to the "false" path.
    else {
        IR_op_to_r(builder, &cond_op);

        IR_Reg imm_reg = IR_next_reg(builder);
        IR_emit_instr_limm(builder, hdr_bb, cond_op.type, imm_reg, ir_zero_imm);

        // Jump that skips the true path if cond == 0.
        IR_Reg cmp_reg = IR_next_reg(builder);
        IR_emit_instr_cmp(builder, hdr_bb, cond_op.type, COND_EQ, cmp_reg, cond_op.reg, imm_reg);
        IR_emit_instr_cond_jmp(builder, hdr_bb, false_tgt, true_bb, cmp_reg);
    }

    // Emit instructions for if-block body.
    BBlock* true_end_bb = IR_emit_stmt(builder, true_bb, if_body, break_target, continue_target);

    // Jump to the end bblock
    IR_emit_instr_jmp(builder, true_end_bb, end_bb); // Not actually needed without else-stmt (fall-through) or if if-stmt returns.
    IR_connect_bblocks(true_end_bb, end_bb);

    if (else_body) {
        BBlock* false_end_bb = IR_emit_stmt(builder, false_bb, else_body, break_target, continue_target);
        IR_emit_instr_jmp(builder, false_end_bb, end_bb); // Not really needed in actual assembly (fall-through)
        IR_connect_bblocks(false_end_bb, end_bb);
    }

    return end_bb;
}

static void IR_emit_inf_loop(IR_ProcBuilder* builder, Stmt* body)
{
    u32 loop_top = IR_get_jmp_target(builder);
    u32* continue_target = IR_alloc_jmp_target(builder, loop_top);
    u32* break_target = IR_alloc_jmp_target(builder, 0);

    // Emit loop body statements.
    IR_emit_stmt(builder, body, break_target, continue_target);

    // Jump back to the top of the loop.
    IR_emit_instr_jmp(builder, IR_alloc_jmp_target(builder, loop_top));

    *break_target = IR_get_jmp_target(builder);
}

static void IR_emit_stmt_while(IR_ProcBuilder* builder, StmtWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_constexpr && cond_expr->is_imm) {
        assert(type_is_scalar(cond_expr->type));
        bool cond_val = cond_expr->imm.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val) {
            IR_emit_inf_loop(builder, body);
        }
    }
    else {
        // Emit jmp instruction to the loop's condition check. Target adddress
        // will be patched.
        Instr* jmp_instr = IR_emit_instr_jmp(builder, IR_alloc_jmp_target(builder, 0));

        // Save the current instruction index to enable jumps to the top of the loop.
        u32 loop_top = IR_get_jmp_target(builder);

        // Emit instructions for the loop body.
        u32* break_target = IR_alloc_jmp_target(builder, 0);
        u32* continue_target = IR_alloc_jmp_target(builder, 0);
        IR_emit_stmt(builder, body, break_target, continue_target);

        // Patch both the initial jmp instruction and the continue target with the location of the condition check.
        u32 loop_cond_check = IR_get_jmp_target(builder);
        IR_patch_jmp_target(jmp_instr, loop_cond_check);
        *continue_target = loop_cond_check;

        // Emit condition expression.
        IR_Operand cond_op = {0};
        IR_emit_expr(builder, cond_expr, &cond_op);

        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP) {

            // Patch final jump to the top of the loop (create one if jump does not exist).
            if (cond_op.cmp.final_jmp.jmp) {
                IR_patch_jmp_target(cond_op.cmp.final_jmp.jmp, loop_top);
            }
            else {
                cond_op.cmp.final_jmp.jmp =
                    IR_emit_instr_cond_jmp(builder, IR_alloc_jmp_target(builder, loop_top), cond_op.cmp.final_jmp.cmp->cmp.r);
            }

            // Reverse jump condition so that it goes to the "true" path.
            IR_DeferredJmpcc* final_jmp = &cond_op.cmp.final_jmp;
            IR_fix_sc_jmp_path(final_jmp, true);

            u32 loop_bottom = IR_get_jmp_target(builder);

            // Patch short-circuit jumps to the top or bottom of the loop.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next) {
                if (it->result)
                    IR_patch_jmp_target(it->jmp, loop_top);
                else
                    IR_patch_jmp_target(it->jmp, loop_bottom);
            }
        }
        else {
            IR_op_to_r(builder, &cond_op);

            // Load zero into a register.
            IR_Reg imm_reg = IR_next_reg(builder);
            IR_emit_instr_limm(builder, cond_op.type, imm_reg, ir_zero_imm);

            // Compare condition expression to zero.
            IR_Reg cmp_reg = IR_next_reg(builder);
            IR_emit_instr_cmp(builder, cond_op.type, COND_NEQ, cmp_reg, cond_op.reg, imm_reg);

            // Emit conditional jump to the top of the loop.
            IR_emit_instr_cond_jmp(builder, IR_alloc_jmp_target(builder, loop_top), cmp_reg);
        }

        *break_target = IR_get_jmp_target(builder);
    }
}

static void IR_emit_stmt_do_while(IR_ProcBuilder* builder, StmtDoWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_constexpr && cond_expr->is_imm) {
        assert(type_is_scalar(cond_expr->type));
        bool cond_val = cond_expr->imm.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val) {
            IR_emit_inf_loop(builder, body);
        }
    }
    else {
        // Save the current instruction index to enable jumps to the top of the loop.
        u32 loop_top = IR_get_jmp_target(builder);

        // Emit instructions for the loop body.
        u32* break_target = IR_alloc_jmp_target(builder, 0);
        u32* continue_target = IR_alloc_jmp_target(builder, 0);
        IR_emit_stmt(builder, body, break_target, continue_target);

        // Patch continue target to the location of the condition check.
        *continue_target = IR_get_jmp_target(builder);

        // Emit condition expression.
        IR_Operand cond_op = {0};
        IR_emit_expr(builder, cond_expr, &cond_op);

        if (cond_op.kind == IR_OPERAND_DEFERRED_CMP) {

            // Path final jump to the top of the loop (create one if jump does not exist).
            if (cond_op.cmp.final_jmp.jmp) {
                IR_patch_jmp_target(cond_op.cmp.final_jmp.jmp, loop_top);
            }
            else {
                cond_op.cmp.final_jmp.jmp =
                    IR_emit_instr_cond_jmp(builder, IR_alloc_jmp_target(builder, loop_top), cond_op.cmp.final_jmp.cmp->cmp.r);
            }

            // Reverse jump condition so that it goes to the "true" path.
            IR_DeferredJmpcc* final_jmp = &cond_op.cmp.final_jmp;
            IR_fix_sc_jmp_path(final_jmp, true);

            u32 loop_bottom = IR_get_jmp_target(builder);

            // Patch short-circuit jumps to the top or bottom of the loop.
            for (IR_DeferredJmpcc* it = cond_op.cmp.first_sc_jmp; it; it = it->next) {
                if (it->result)
                    IR_patch_jmp_target(it->jmp, loop_top);
                else
                    IR_patch_jmp_target(it->jmp, loop_bottom);
            }
        }
        else {
            IR_op_to_r(builder, &cond_op);

            // Load zero into a register.
            IR_Reg imm_reg = IR_next_reg(builder);
            IR_emit_instr_limm(builder, cond_op.type, imm_reg, ir_zero_imm);

            // Compare condition expression to zero.
            IR_Reg cmp_reg = IR_next_reg(builder);
            IR_emit_instr_cmp(builder, cond_op.type, COND_NEQ, cmp_reg, cond_op.reg, imm_reg);

            // Emit conditional jump to the top of the loop.
            IR_emit_instr_cond_jmp(builder, IR_alloc_jmp_target(builder, loop_top), cmp_reg);
        }

        *break_target = IR_get_jmp_target(builder);
    }
}

static void IR_emit_stmt(IR_ProcBuilder* builder, Stmt* stmt, u32* break_target, u32* continue_target)
{
    switch (stmt->kind) {
    case CST_StmtBlock:
        IR_emit_stmt_block(builder, (StmtBlock*)stmt, break_target, continue_target);
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
        IR_emit_stmt_if(builder, (StmtIf*)stmt, break_target, continue_target);
        break;
    case CST_StmtWhile:
        IR_emit_stmt_while(builder, (StmtWhile*)stmt);
        break;
    case CST_StmtDoWhile:
        IR_emit_stmt_do_while(builder, (StmtDoWhile*)stmt);
        break;
    case CST_StmtBreak:
        IR_emit_instr_jmp(builder, break_target);
        break;
    case CST_StmtContinue:
        IR_emit_instr_jmp(builder, continue_target);
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

static void IR_build_proc(IR_ProcBuilder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    if (dproc->is_incomplete) {
        return;
    }

    // Set procedure as the current scope.
    IR_push_scope(builder, dproc->scope);
    builder->curr_proc = sym;

    sym->as_proc.instrs = array_create(builder->arena, Instr*, 32);

    IR_emit_stmt_block_body(builder, &dproc->stmts, NULL, NULL);

    // If proc doesn't have explicit returns, add one at the end.
    // NOTE: This should only apply to procs that return void. The resolver
    // will catch other cases.
    if (!dproc->returns) {
        assert(sym->type->as_proc.ret == builtin_types[BUILTIN_TYPE_VOID].type);
        IR_emit_instr_ret(builder, builtin_types[BUILTIN_TYPE_VOID].type, IR_REG_COUNT);
    }

    IR_pop_scope(builder);
    builder->curr_proc = NULL;

#ifdef NIBBLE_PRINT_DECLS
    IR_print_out_proc(builder->tmp_arena, sym);
#endif
}

static void IR_build_procs(Allocator* arena, Allocator* tmp_arena, BucketList* procs, TypeCache* type_cache)
{
    IR_ProcBuilder builder =
        {.arena = arena, .tmp_arena = tmp_arena, .type_cache = type_cache, .curr_proc = NULL, .curr_scope = NULL};

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
