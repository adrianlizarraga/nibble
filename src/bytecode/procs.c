#include "ast/module.h"
#include "bytecode/module.h"

typedef struct IR_ProcBuilder {
    Allocator* arena;
    Allocator* tmp_arena;
    GlobalData* str_lits;
    GlobalData* float_lits;
    TypeCache* type_cache;
    HMap* float_lit_map;
    Symbol* curr_proc;
    Scope* curr_scope;
    List* curr_tmp_obj;

    FloatLit* zero_flts[FLOAT_KIND_COUNT];

    struct IR_TmpObj* tmp_obj_freelist;
    struct IR_DeferredJmpcc* sc_jmp_freelist;
    struct IR_UJmpNode* ujmp_freelist;
} IR_ProcBuilder;

typedef enum IR_ExprResultKind {
    IR_EXPR_RESULT_NONE,
    IR_EXPR_RESULT_IMM,
    IR_EXPR_RESULT_REG,
    IR_EXPR_RESULT_MEM_ADDR,
    IR_EXPR_RESULT_DEREF_ADDR,
    IR_EXPR_RESULT_DEFERRED_CMP,
    IR_EXPR_RESULT_VAR,
    IR_EXPR_RESULT_TMP_OBJ,
    IR_EXPR_RESULT_STR_LIT,
    IR_EXPR_RESULT_FLOAT_LIT,
    IR_EXPR_RESULT_PROC,
} IR_ExprResultKind;

typedef struct IR_TmpObj {
    size_t size;
    size_t align;
    MemObj* mem_obj;
    struct IR_TmpObj* next;
} IR_TmpObj;

typedef struct IR_TmpObjList {
    IR_TmpObj* first;
    IR_TmpObj* last;
} IR_TmpObjList;

typedef struct IR_DeferredJmpcc {
    bool result;
    IR_Instr* jmp; // target needs to be patched. Cond reg needs to be set.
    IR_Instr* cmp; // Condition may need to be inverted.
    struct IR_DeferredJmpcc* next;
} IR_DeferredJmpcc;

typedef struct IR_DeferredCmp {
    IR_DeferredJmpcc* first_sc_jmp;
    IR_DeferredJmpcc* last_sc_jmp;
    IR_DeferredJmpcc final_jmp;
} IR_DeferredCmp;

typedef struct IR_ExprResult {
    IR_ExprResultKind kind;
    Type* type;

    union {
        Scalar imm;
        IR_Reg reg;
        MemAddr addr;
        Symbol* sym;
        IR_TmpObj* tmp_obj;
        IR_DeferredCmp cmp;
        StrLit* str_lit;
        FloatLit* float_lit;
    };
} IR_ExprResult;

static const Scalar ir_zero_imm = {.as_int._u64 = 0};
static const Scalar ir_one_imm = {.as_int._u64 = 1};

static const ConditionKind ir_opposite_cond[] = {
    [COND_U_LT] = COND_U_GTEQ, [COND_S_LT] = COND_S_GTEQ, [COND_U_LTEQ] = COND_U_GT, [COND_S_LTEQ] = COND_S_GT,
    [COND_U_GT] = COND_U_LTEQ, [COND_S_GT] = COND_S_LTEQ, [COND_U_GTEQ] = COND_U_LT, [COND_S_GTEQ] = COND_S_LT,
    [COND_EQ] = COND_NEQ,      [COND_NEQ] = COND_EQ,
};

static MemAddr IR_sym_as_addr(IR_ProcBuilder* builder, Symbol* sym)
{
    MemObj* mem_obj = alloc_type(builder->arena, MemObj, false);
    mem_obj->kind = MEM_OBJ_SYM;
    mem_obj->sym = sym;

    MemAddr addr = {.base_kind = MEM_BASE_MEM_OBJ, .base.obj = mem_obj, .index_reg = IR_REG_COUNT};
    return addr;
}

static MemAddr IR_tmp_obj_as_addr(IR_TmpObj* obj)
{
    MemAddr addr = {.base_kind = MEM_BASE_MEM_OBJ, .base.obj = obj->mem_obj, .index_reg = IR_REG_COUNT};
    return addr;
}

static MemAddr IR_strlit_as_addr(StrLit* str_lit)
{
    MemAddr addr = {.base_kind = MEM_BASE_STR_LIT, .base.str_lit = str_lit, .index_reg = IR_REG_COUNT};
    return addr;
}

static MemAddr IR_floatlit_as_addr(FloatLit* float_lit)
{
    MemAddr addr = {.base_kind = MEM_BASE_FLOAT_LIT, .base.float_lit = float_lit, .index_reg = IR_REG_COUNT};
    return addr;
}

static inline OpRI op_ri_from_imm(Scalar imm)
{
    return (OpRI){.is_imm = true, .imm = imm};
}

static inline OpRIA op_ria_from_reg(IR_Reg reg)
{
    return (OpRIA){.kind = OP_RIA_REG, .reg = reg};
}

static inline OpRIA op_ria_from_imm(Scalar imm)
{
    return (OpRIA){.kind = OP_RIA_IMM, .imm = imm};
}

static MemAddr IR_get_zero_flt_addr(IR_ProcBuilder* builder, FloatKind fkind)
{
    FloatLit* float_lit = builder->zero_flts[fkind];

    if (!float_lit) {
        Float flt = {0};
        float_lit = intern_float_lit(builder->float_lit_map, fkind, flt);

        builder->zero_flts[fkind] = float_lit;
    }

    return IR_floatlit_as_addr(float_lit);
}

//////////////////////////////////////////////////////
//
//         Create IR instructions
//
//////////////////////////////////////////////////////
typedef struct IR_UJmpNode {
    IR_Instr* instr; // Unpatched jump instruction.
    struct IR_UJmpNode* next;
} IR_UJmpNode;

typedef struct IR_UJmpList {
    IR_UJmpNode* first;
    IR_UJmpNode* last;
} IR_UJmpList;

static void IR_add_ujmp(IR_ProcBuilder* builder, IR_UJmpList* list, IR_Instr* instr)
{
    IR_UJmpNode* node;

    // Try to get a node from the free list. Otherwise, just allocate one.
    if (builder->ujmp_freelist) {
        node = builder->ujmp_freelist;
        builder->ujmp_freelist = node->next;
    }
    else {
        node = alloc_type(builder->tmp_arena, IR_UJmpNode, false);
    }

    // Init node data
    node->instr = instr;
    node->next = NULL;

    // Add node to the end of the list.
    if (list->last) {
        list->last->next = node;
    }
    else {
        list->first = node;
    }

    list->last = node;
}

static void IR_ujmp_list_free(IR_ProcBuilder* builder, IR_UJmpList* list)
{
    if (!list->last) {
        return;
    }

    // Add entire list to the free list.
    list->last->next = builder->ujmp_freelist;
    builder->ujmp_freelist = list->first;

    // Clear out list pointers.
    list->first = list->last = NULL;
}

static void IR_bblock_add_instr(BBlock* bblock, IR_Instr* instr)
{
    assert(!bblock->closed);

    if (!bblock->first) {
        bblock->first = instr;
    }
    else {
        bblock->last->next = instr;
    }

    bblock->last = instr;
    bblock->num_instrs += 1;

    if (instr->kind == IR_InstrJmp_KIND || instr->kind == IR_InstrCondJmp_KIND || instr->kind == IR_InstrRet_KIND) {
        bblock->closed = true;
    }
}

static BBlock* IR_alloc_bblock(IR_ProcBuilder* builder)
{
    BBlock* block = alloc_type(builder->arena, BBlock, true);

    block->preds = array_create(builder->arena, BBlock*, 4);
    block->id = array_len(builder->curr_proc->as_proc.bblocks);
    array_push(builder->curr_proc->as_proc.bblocks, block);

    return block;
}

static void IR_try_push_bblock_elem(BBlock*** p_array, BBlock* bblock)
{
#ifdef DEBUG
    bool found = false;
    size_t len = array_len(*p_array);

    for (size_t i = 0; i < len; i++) {
        if ((*p_array)[i] == bblock) {
            found = true;
            break;
        }
    }

    assert(!found);
#endif

    array_push(*p_array, bblock);
}

static void IR_connect_bblocks(BBlock* pred, BBlock* succ)
{
    IR_try_push_bblock_elem(&succ->preds, pred);
}

static void IR_patch_jmp_target(IR_Instr* jmp_instr, BBlock* target)
{
    assert(target);

    switch (jmp_instr->kind) {
    case IR_InstrJmp_KIND: {
        IR_InstrJmp* instr = (IR_InstrJmp*)jmp_instr;
        instr->target = target;
        IR_connect_bblocks(instr->from, target);
        break;
    }
    case IR_InstrCondJmp_KIND: {
        IR_InstrCondJmp* instr = (IR_InstrCondJmp*)jmp_instr;
        instr->true_bb = target; // NOTE: Only patches true path
        IR_connect_bblocks(instr->from, target);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void IR_patch_ujmp_list(IR_ProcBuilder* builder, IR_UJmpList* list, BBlock* target)
{
    for (IR_UJmpNode* it = list->first; it; it = it->next) {
        IR_patch_jmp_target(it->instr, target);
    }

    IR_ujmp_list_free(builder, list);
}

static void IR_add_instr(IR_ProcBuilder* builder, BBlock* bblock, IR_Instr* instr)
{
    instr->ino = builder->curr_proc->as_proc.num_instrs << 1;
    builder->curr_proc->as_proc.num_instrs++;
    IR_bblock_add_instr(bblock, instr);
}

#define IR_new_instr(a, k) (k*)IR_new_instr_((a), k##_KIND, sizeof(k), alignof(k))
static IR_Instr* IR_new_instr_(Allocator* arena, IR_InstrKind kind, size_t size, size_t align)
{
    IR_Instr* instr = mem_allocate(arena, size, align, true);
    instr->kind = kind;

    return instr;
}

static void IR_emit_instr_int_add(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrIntAdd* instr = IR_new_instr(builder->arena, IR_InstrIntAdd);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_int_sub(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrIntSub* instr = IR_new_instr(builder->arena, IR_InstrIntSub);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_int_mul(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrIntMul* instr = IR_new_instr(builder->arena, IR_InstrIntMul);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_int_div(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrIntDiv* instr = IR_new_instr(builder->arena, IR_InstrIntDiv);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_and(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrAnd* instr = IR_new_instr(builder->arena, IR_InstrAnd);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_or(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrOr* instr = IR_new_instr(builder->arena, IR_InstrOr);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_xor(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrXor* instr = IR_new_instr(builder->arena, IR_InstrXor);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_flt_add(IR_ProcBuilder* builder, BBlock* bblock, FloatKind fkind, IR_Reg r, OpRA a, OpRA b)
{
    IR_InstrFltAdd* instr = IR_new_instr(builder->arena, IR_InstrFltAdd);
    instr->fkind = fkind;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_flt_sub(IR_ProcBuilder* builder, BBlock* bblock, FloatKind fkind, IR_Reg r, OpRA a, OpRA b)
{
    IR_InstrFltSub* instr = IR_new_instr(builder->arena, IR_InstrFltSub);
    instr->fkind = fkind;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_flt_mul(IR_ProcBuilder* builder, BBlock* bblock, FloatKind fkind, IR_Reg r, OpRA a, OpRA b)
{
    IR_InstrFltMul* instr = IR_new_instr(builder->arena, IR_InstrFltMul);
    instr->fkind = fkind;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_flt_div(IR_ProcBuilder* builder, BBlock* bblock, FloatKind fkind, IR_Reg r, OpRA a, OpRA b)
{
    IR_InstrFltDiv* instr = IR_new_instr(builder->arena, IR_InstrFltDiv);
    instr->fkind = fkind;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_sar(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrSar* instr = IR_new_instr(builder->arena, IR_InstrSar);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_shl(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    IR_InstrShl* instr = IR_new_instr(builder->arena, IR_InstrShl);
    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_mod(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, OpRIA a, OpRIA b)
{
    assert(type->kind == TYPE_INTEGER);

    IR_InstrMod* instr = IR_new_instr(builder->arena, IR_InstrMod);

    instr->type = type;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_divmod(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg q, IR_Reg r, OpRIA a, OpRIA b)
{
    assert(type->kind == TYPE_INTEGER);

    IR_InstrDivMod* instr = IR_new_instr(builder->arena, IR_InstrDivMod);

    instr->type = type;
    instr->q = q;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_not(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, IR_Reg a)
{
    IR_InstrNot* instr = IR_new_instr(builder->arena, IR_InstrNot);
    instr->type = type;
    instr->r = r;
    instr->a = a;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_neg(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, IR_Reg a)
{
    IR_InstrNeg* instr = IR_new_instr(builder->arena, IR_InstrNeg);
    instr->type = type;
    instr->r = r;
    instr->a = a;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_trunc(IR_ProcBuilder* builder, BBlock* bblock, Type* dst_type, Type* src_type, IR_Reg r, IR_Reg a)
{
    IR_InstrTrunc* instr = IR_new_instr(builder->arena, IR_InstrTrunc);
    instr->dst_type = dst_type;
    instr->src_type = src_type;
    instr->r = r;
    instr->a = a;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_zext(IR_ProcBuilder* builder, BBlock* bblock, Type* dst_type, Type* src_type, IR_Reg r, IR_Reg a)
{
    IR_InstrZExt* instr = IR_new_instr(builder->arena, IR_InstrZExt);
    instr->dst_type = dst_type;
    instr->src_type = src_type;
    instr->r = r;
    instr->a = a;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_sext(IR_ProcBuilder* builder, BBlock* bblock, Type* dst_type, Type* src_type, IR_Reg r, IR_Reg a)
{
    IR_InstrSExt* instr = IR_new_instr(builder->arena, IR_InstrSExt);
    instr->dst_type = dst_type;
    instr->src_type = src_type;
    instr->r = r;
    instr->a = a;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_flt2int(IR_ProcBuilder* builder, BBlock* bblock, IntegerKind dst_kind, FloatKind src_kind, IR_Reg dst,
                                  OpRA src)
{
    IR_InstrFlt2Int* instr = IR_new_instr(builder->arena, IR_InstrFlt2Int);
    instr->dst_kind = dst_kind;
    instr->src_kind = src_kind;
    instr->dst = dst;
    instr->src = src;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_int2flt(IR_ProcBuilder* builder, BBlock* bblock, FloatKind dst_kind, IntegerKind src_kind, IR_Reg dst,
                                  OpRA src)
{
    IR_InstrInt2Flt* instr = IR_new_instr(builder->arena, IR_InstrInt2Flt);
    instr->dst_kind = dst_kind;
    instr->src_kind = src_kind;
    instr->dst = dst;
    instr->src = src;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_flt2flt(IR_ProcBuilder* builder, BBlock* bblock, FloatKind dst_kind, FloatKind src_kind, IR_Reg dst,
                                  OpRA src)
{
    IR_InstrFlt2Flt* instr = IR_new_instr(builder->arena, IR_InstrFlt2Flt);
    instr->dst_kind = dst_kind;
    instr->src_kind = src_kind;
    instr->dst = dst;
    instr->src = src;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_limm(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, Scalar imm)
{
    IR_InstrLImm* instr = IR_new_instr(builder->arena, IR_InstrLImm);
    instr->type = type;
    instr->r = r;
    instr->imm = imm;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_load(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, MemAddr addr)
{
    IR_InstrLoad* instr = IR_new_instr(builder->arena, IR_InstrLoad);
    instr->type = type;
    instr->r = r;
    instr->addr = addr;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_laddr(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, MemAddr addr)
{
    IR_InstrLAddr* instr = IR_new_instr(builder->arena, IR_InstrLAddr);
    instr->type = type;
    instr->r = r;
    instr->addr = addr;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_store(IR_ProcBuilder* builder, BBlock* bblock, Type* type, MemAddr addr, OpRI a)
{
    IR_InstrStore* instr = IR_new_instr(builder->arena, IR_InstrStore);
    instr->type = type;
    instr->addr = addr;
    instr->a = a;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static IR_Instr* IR_emit_instr_int_cmp(IR_ProcBuilder* builder, BBlock* bblock, Type* type, ConditionKind cond, IR_Reg r, OpRIA a,
                                       OpRIA b)
{
    IR_InstrIntCmp* instr = IR_new_instr(builder->arena, IR_InstrIntCmp);
    instr->type = type;
    instr->cond = cond;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);

    return (IR_Instr*)instr;
}

static IR_Instr* IR_emit_instr_flt_cmp(IR_ProcBuilder* builder, BBlock* bblock, FloatKind fkind, ConditionKind cond, IR_Reg r,
                                       IR_Reg a, OpRA b)
{
    IR_InstrFltCmp* instr = IR_new_instr(builder->arena, IR_InstrFltCmp);
    instr->fkind = fkind;
    instr->cond = cond;
    instr->r = r;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);

    return (IR_Instr*)instr;
}

static IR_Instr* IR_emit_instr_cond_jmp(IR_ProcBuilder* builder, BBlock* bblock, BBlock* true_bb, BBlock* false_bb, IR_Reg a)
{
    IR_InstrCondJmp* instr = IR_new_instr(builder->arena, IR_InstrCondJmp);
    instr->from = bblock;
    instr->true_bb = true_bb;
    instr->false_bb = false_bb;
    instr->a = a;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);

    if (true_bb) {
        IR_connect_bblocks(bblock, true_bb);
    }

    if (false_bb) {
        IR_connect_bblocks(bblock, false_bb);
    }

    return (IR_Instr*)instr;
}

static IR_Instr* IR_emit_instr_jmp(IR_ProcBuilder* builder, BBlock* bblock, BBlock* target)
{
    IR_InstrJmp* instr = IR_new_instr(builder->arena, IR_InstrJmp);
    instr->from = bblock;
    instr->target = target;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);

    if (target) {
        IR_connect_bblocks(bblock, target);
    }

    return (IR_Instr*)instr;
}

static void IR_emit_instr_call(IR_ProcBuilder* builder, BBlock* bblock, Symbol* sym, IR_Value r, u32 num_args, IR_Value* args)
{
    IR_InstrCall* instr = IR_new_instr(builder->arena, IR_InstrCall);
    instr->sym = sym;
    instr->r = r;
    instr->num_args = num_args;
    instr->args = args;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_call_indirect(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg loc, IR_Value r, u32 num_args,
                                        IR_Value* args)
{
    IR_InstrCallIndirect* instr = IR_new_instr(builder->arena, IR_InstrCallIndirect);
    instr->proc_type = type;
    instr->loc = loc;
    instr->r = r;
    instr->num_args = num_args;
    instr->args = args;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_ret(IR_ProcBuilder* builder, BBlock* bblock, IR_Value val)
{
    IR_InstrRet* instr = IR_new_instr(builder->arena, IR_InstrRet);
    instr->val = val;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_memcpy(IR_ProcBuilder* builder, BBlock* bblock, MemAddr dst, MemAddr src, OpRI size)
{
    IR_InstrMemcpy* instr = IR_new_instr(builder->arena, IR_InstrMemcpy);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_memset(IR_ProcBuilder* builder, BBlock* bblock, MemAddr dst, OpRI value, OpRI size)
{
    IR_InstrMemset* instr = IR_new_instr(builder->arena, IR_InstrMemset);
    instr->dst = dst;
    instr->value = value;
    instr->size = size;

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_syscall(IR_ProcBuilder* builder, BBlock* bblock, IR_Reg r, OpRIA syscall_nr, u8 num_args, OpRIA* args)
{
    assert(num_args <= 6);
    IR_InstrSyscall* instr = IR_new_instr(builder->arena, IR_InstrSyscall);
    instr->r = r;
    instr->nr = syscall_nr;
    instr->count = num_args;
    instr->args = mem_dup_array(builder->arena, OpRIA, args, num_args);

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
}

static void IR_emit_instr_phi(IR_ProcBuilder* builder, BBlock* bblock, Type* type, IR_Reg r, size_t num_args, PhiArg* args)
{
    IR_InstrPhi* instr = IR_new_instr(builder->arena, IR_InstrPhi);
    instr->type = type;
    instr->r = r;
    instr->num_args = num_args;
    instr->args = mem_dup_array(builder->arena, PhiArg, args, num_args);

    IR_add_instr(builder, bblock, (IR_Instr*)instr);
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

static void IR_get_object_addr(IR_ProcBuilder* builder, BBlock* bblock, MemAddr* dst, IR_ExprResult* src)
{
    Type* src_type = src->type;

    if (src->kind == IR_EXPR_RESULT_VAR) {
        if (src->sym->is_local) {
            *dst = IR_sym_as_addr(builder, src->sym);
        }
        else {
            IR_Reg dst_reg = IR_next_reg(builder);
            IR_emit_instr_laddr(builder, bblock, src_type, dst_reg, IR_sym_as_addr(builder, src->sym));

            dst->base_kind = MEM_BASE_REG;
            dst->base.reg = dst_reg;
            dst->index_reg = IR_REG_COUNT;
            dst->disp = 0;
            dst->scale = 0;
        }
    }
    else if (src->kind == IR_EXPR_RESULT_DEREF_ADDR) {
        *dst = src->addr;
    }
    else if (src->kind == IR_EXPR_RESULT_TMP_OBJ) {
        *dst = IR_tmp_obj_as_addr(src->tmp_obj);
    }
    else {
        assert(src->kind == IR_EXPR_RESULT_STR_LIT);

        IR_Reg dst_reg = IR_next_reg(builder);
        IR_emit_instr_laddr(builder, bblock, src_type, dst_reg, IR_strlit_as_addr(src->str_lit));

        dst->base_kind = MEM_BASE_REG;
        dst->base.reg = dst_reg;
        dst->index_reg = IR_REG_COUNT;
        dst->disp = 0;
        dst->scale = 0;
    }
}

static void IR_expr_result_from_sym(IR_ExprResult* op, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR || sym->kind == SYMBOL_PROC);
    op->kind = (sym->kind == SYMBOL_VAR) ? IR_EXPR_RESULT_VAR : IR_EXPR_RESULT_PROC;
    op->type = sym->type;
    op->sym = sym;
}

static void IR_new_deferred_sc_jmp(IR_ProcBuilder* builder, IR_DeferredCmp* cmp, IR_Instr* cmp_instr, bool result, IR_Instr* jmp_instr)
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
    IR_Instr* instr = def_jmp->cmp;
    ConditionKind* p_cond = instr->kind == IR_InstrIntCmp_KIND ? &((IR_InstrIntCmp*)instr)->cond : &((IR_InstrFltCmp*)instr)->cond;

    if (def_jmp->result != desired_result) {
        *p_cond = ir_opposite_cond[*p_cond];
        def_jmp->result = desired_result;
    }
}

static BBlock* IR_copy_sc_jmp(IR_ProcBuilder* builder, BBlock* bblock, IR_DeferredJmpcc* dst_jmp, IR_DeferredJmpcc* src_jmp,
                              bool desired_result)
{
    *dst_jmp = *src_jmp;

    IR_fix_sc_jmp_path(dst_jmp, desired_result);

    if (dst_jmp->jmp) {
        return bblock;
    }

    BBlock* last_bb = IR_alloc_bblock(builder);
    IR_Instr* cmp_instr = dst_jmp->cmp;
    IR_Reg cmp_result = cmp_instr->kind == IR_InstrIntCmp_KIND ? ((IR_InstrIntCmp*)cmp_instr)->r : ((IR_InstrFltCmp*)cmp_instr)->r;

    dst_jmp->jmp = IR_emit_instr_cond_jmp(builder, bblock, NULL, last_bb, cmp_result);

    return last_bb;
}

static BBlock* IR_execute_deferred_cmp(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* expr_result)
{
    assert(expr_result->kind == IR_EXPR_RESULT_DEFERRED_CMP);
    assert(type_is_bool(expr_result->type));

    BBlock* e_bblock;
    IR_DeferredCmp* def_cmp = &expr_result->cmp;
    IR_Reg dst_reg;

    bool has_sc_jmps = def_cmp->first_sc_jmp != NULL;
    bool has_final_jmp = def_cmp->final_jmp.jmp != NULL;

    if (!has_sc_jmps && !has_final_jmp) {
        e_bblock = bblock;

        IR_Instr* cmp_instr = def_cmp->final_jmp.cmp;
        dst_reg = cmp_instr->kind == IR_InstrIntCmp_KIND ? ((IR_InstrIntCmp*)cmp_instr)->r : ((IR_InstrFltCmp*)cmp_instr)->r;
    }
    else {
        // Fix final jmp condition so that it jumps to "false" control path.
        IR_DeferredJmpcc* final_jmp = &def_cmp->final_jmp;
        IR_fix_sc_jmp_path(final_jmp, false);

        BBlock* t_bblock = bblock;
        BBlock* f_bblock = IR_alloc_bblock(builder);
        e_bblock = IR_alloc_bblock(builder);

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
        IR_emit_instr_limm(builder, t_bblock, expr_result->type, one_reg, ir_one_imm);

        // Create a jump to skip the false control path.
        IR_emit_instr_jmp(builder, t_bblock, e_bblock);

        //
        // False control path.
        //

        assert(final_jmp->jmp);
        IR_patch_jmp_target(final_jmp->jmp, f_bblock);

        // Patch short-circuit jumps that jump to the "false" control path.
        for (IR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next) {
            if (!it->result)
                IR_patch_jmp_target(it->jmp, f_bblock);
        }

        // This is the "false" control path. Move the literal 0 into destination register.
        IR_Reg zero_reg = IR_next_reg(builder);
        IR_emit_instr_limm(builder, f_bblock, expr_result->type, zero_reg, ir_zero_imm);
        IR_emit_instr_jmp(builder, f_bblock, e_bblock); // NOTE: Not needed in actual assembly (fall-through)

        //
        // End block.
        //

        // Emit PHI instruction.
        dst_reg = IR_next_reg(builder);
        PhiArg phi_args[2] = {{.bblock = t_bblock, .ireg = one_reg}, {.bblock = f_bblock, .ireg = zero_reg}};

        IR_emit_instr_phi(builder, e_bblock, expr_result->type, dst_reg, 2, phi_args);
    }

    expr_result->kind = IR_EXPR_RESULT_REG;
    expr_result->reg = dst_reg;

    return e_bblock;
}

static void IR_execute_deref(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* expr_result)
{
    assert(expr_result->kind == IR_EXPR_RESULT_DEREF_ADDR);

    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_load(builder, bblock, expr_result->type, dst_reg, expr_result->addr);

    expr_result->kind = IR_EXPR_RESULT_REG;
    expr_result->reg = dst_reg;
}

static void IR_execute_lea(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* expr_result)
{
    assert(expr_result->kind == IR_EXPR_RESULT_MEM_ADDR);

    // The expr_result currently holds a memory address.
    // This function executes the "load-effective-address" call.
    MemAddr addr = expr_result->addr;
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
        IR_emit_instr_laddr(builder, bblock, expr_result->type, dst_reg, addr);
    }

    expr_result->kind = IR_EXPR_RESULT_REG;
    expr_result->reg = dst_reg;
}

static void IR_ptr_to_mem_expr_result(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* expr_result)
{
    assert(expr_result->type->kind == TYPE_PTR);

    if (expr_result->kind == IR_EXPR_RESULT_MEM_ADDR) {
        return;
    }

    IR_Reg base_reg = IR_REG_COUNT;

    if (expr_result->kind == IR_EXPR_RESULT_VAR) {
        base_reg = IR_next_reg(builder);
        IR_emit_instr_load(builder, bblock, expr_result->type, base_reg, IR_sym_as_addr(builder, expr_result->sym));
    }
    else if (expr_result->kind == IR_EXPR_RESULT_DEREF_ADDR) {
        // Occurs when dereferencing a pointer to a pointer.
        // Ex:
        //     var p  : ^char = "hi";
        //     var pp : ^^char = ^p;
        //     #writeout(*pp + 1, 1); // Happens for expression `*pp + 1`
        IR_execute_deref(builder, bblock, expr_result);
        base_reg = expr_result->reg;
    }
    else if (expr_result->kind == IR_EXPR_RESULT_REG) {
        base_reg = expr_result->reg;
    }

    assert(base_reg != IR_REG_COUNT);

    expr_result->kind = IR_EXPR_RESULT_MEM_ADDR;
    expr_result->addr.base_kind = MEM_BASE_REG;
    expr_result->addr.base.reg = base_reg;
    expr_result->addr.index_reg = IR_REG_COUNT;
    expr_result->addr.scale = 0;
    expr_result->addr.disp = 0;
}

static BBlock* IR_expr_result_to_reg(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* expr_result)
{
    if (expr_result->kind == IR_EXPR_RESULT_REG) {
        return bblock;
    }

    switch (expr_result->kind) {
    case IR_EXPR_RESULT_MEM_ADDR:
        IR_execute_lea(builder, bblock, expr_result);
        return bblock;
    case IR_EXPR_RESULT_DEREF_ADDR:
        IR_execute_deref(builder, bblock, expr_result);
        return bblock;
    case IR_EXPR_RESULT_DEFERRED_CMP:
        return IR_execute_deferred_cmp(builder, bblock, expr_result);
    case IR_EXPR_RESULT_IMM: {
        IR_Reg reg = IR_next_reg(builder);

        IR_emit_instr_limm(builder, bblock, expr_result->type, reg, expr_result->imm);

        expr_result->kind = IR_EXPR_RESULT_REG;
        expr_result->reg = reg;

        return bblock;
    }
    case IR_EXPR_RESULT_PROC: {
        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_laddr(builder, bblock, expr_result->type, reg, IR_sym_as_addr(builder, expr_result->sym));

        expr_result->kind = IR_EXPR_RESULT_REG;
        expr_result->reg = reg;

        return bblock;
    }
    case IR_EXPR_RESULT_TMP_OBJ: {
        assert(IR_type_fits_in_reg(expr_result->type));

        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_load(builder, bblock, expr_result->type, reg, IR_tmp_obj_as_addr(expr_result->tmp_obj));

        expr_result->kind = IR_EXPR_RESULT_REG;
        expr_result->reg = reg;

        return bblock;
    }
    case IR_EXPR_RESULT_STR_LIT: {
        assert(IR_type_fits_in_reg(expr_result->type));

        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_load(builder, bblock, expr_result->type, reg, IR_strlit_as_addr(expr_result->str_lit));

        expr_result->kind = IR_EXPR_RESULT_REG;
        expr_result->reg = reg;

        return bblock;
    }
    case IR_EXPR_RESULT_FLOAT_LIT: {
        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_load(builder, bblock, expr_result->type, reg, IR_floatlit_as_addr(expr_result->float_lit));

        expr_result->kind = IR_EXPR_RESULT_REG;
        expr_result->reg = reg;

        return bblock;
    }
    default: {
        assert(expr_result->kind == IR_EXPR_RESULT_VAR);
        IR_Reg reg = IR_next_reg(builder);
        IR_emit_instr_load(builder, bblock, expr_result->type, reg, IR_sym_as_addr(builder, expr_result->sym));

        expr_result->kind = IR_EXPR_RESULT_REG;
        expr_result->reg = reg;

        return bblock;
    }
    }
}

static OpRI IR_expr_result_to_op_ri(IR_ProcBuilder* builder, BBlock** p_bblock, IR_ExprResult* expr_result)
{
    OpRI ri;

    if (expr_result->kind == IR_EXPR_RESULT_REG) {
        ri.is_imm = false;
        ri.reg = expr_result->reg;
    }
    else if (expr_result->kind == IR_EXPR_RESULT_IMM) {
        ri.is_imm = true;
        ri.imm = expr_result->imm;
    }
    else {
        *p_bblock = IR_expr_result_to_reg(builder, *p_bblock, expr_result);
        ri.is_imm = false;
        ri.reg = expr_result->reg;
    }

    return ri;
}

static OpRA IR_expr_result_to_op_ra(IR_ProcBuilder* builder, BBlock** p_bblock, IR_ExprResult* expr_result)
{
    OpRA ra = {0};

    switch (expr_result->kind) {
    case IR_EXPR_RESULT_REG:
        ra.is_addr = false;
        ra.reg = expr_result->reg;
        break;
    case IR_EXPR_RESULT_VAR:
        ra.is_addr = true;
        ra.addr = IR_sym_as_addr(builder, expr_result->sym);
        break;
    case IR_EXPR_RESULT_DEREF_ADDR:
        ra.is_addr = true;
        ra.addr = expr_result->addr;
        break;
    case IR_EXPR_RESULT_TMP_OBJ:
        ra.is_addr = true;
        ra.addr = IR_tmp_obj_as_addr(expr_result->tmp_obj);
        break;
    case IR_EXPR_RESULT_STR_LIT:
        ra.is_addr = true;
        ra.addr = IR_strlit_as_addr(expr_result->str_lit);
        break;
    case IR_EXPR_RESULT_FLOAT_LIT:
        ra.is_addr = true;
        ra.addr = IR_floatlit_as_addr(expr_result->float_lit);
        break;
    case IR_EXPR_RESULT_MEM_ADDR:
    case IR_EXPR_RESULT_DEFERRED_CMP:
    case IR_EXPR_RESULT_PROC:
    case IR_EXPR_RESULT_IMM:
        *p_bblock = IR_expr_result_to_reg(builder, *p_bblock, expr_result);
        ra.is_addr = false;
        ra.reg = expr_result->reg;
        break;
    default:
        NIBBLE_FATAL_EXIT("IR_expr_result_to_op_ra(): Unexpected IR_ExprResultKind %d", expr_result->kind);
    }

    return ra;
}

static OpRIA IR_expr_result_to_op_ria(IR_ProcBuilder* builder, BBlock** p_bblock, IR_ExprResult* expr_result)
{
    OpRIA ria = {0};

    switch (expr_result->kind) {
    case IR_EXPR_RESULT_REG:
        ria.kind = OP_RIA_REG;
        ria.reg = expr_result->reg;
        break;
    case IR_EXPR_RESULT_IMM:
        ria.kind = OP_RIA_IMM;
        ria.imm = expr_result->imm;
        break;
    case IR_EXPR_RESULT_VAR:
        ria.kind = OP_RIA_ADDR;
        ria.addr = IR_sym_as_addr(builder, expr_result->sym);
        break;
    case IR_EXPR_RESULT_DEREF_ADDR:
        ria.kind = OP_RIA_ADDR;
        ria.addr = expr_result->addr;
        break;
    case IR_EXPR_RESULT_TMP_OBJ:
        ria.kind = OP_RIA_ADDR;
        ria.addr = IR_tmp_obj_as_addr(expr_result->tmp_obj);
        break;
    case IR_EXPR_RESULT_STR_LIT:
        ria.kind = OP_RIA_ADDR;
        ria.addr = IR_strlit_as_addr(expr_result->str_lit);
        break;
    case IR_EXPR_RESULT_FLOAT_LIT:
        ria.kind = OP_RIA_ADDR;
        ria.addr = IR_floatlit_as_addr(expr_result->float_lit);
        break;
    case IR_EXPR_RESULT_MEM_ADDR:
    case IR_EXPR_RESULT_DEFERRED_CMP:
    case IR_EXPR_RESULT_PROC:
        *p_bblock = IR_expr_result_to_reg(builder, *p_bblock, expr_result);
        ria.kind = OP_RIA_REG;
        ria.reg = expr_result->reg;
        break;
    default:
        NIBBLE_FATAL_EXIT("IR_expr_result_to_op_ria(): Unexpected IR_ExprResultKind %d", expr_result->kind);
    }

    return ria;
}

static IR_Instr* IR_cmp_to_zero(IR_ProcBuilder* builder, BBlock** p_bblock, IR_ExprResult* src_er, ConditionKind cond)
{
    IR_Instr* cmp_instr = NULL;
    Type* type = src_er->type;

    if (type_is_int_scalar(type)) {
        OpRIA a = IR_expr_result_to_op_ria(builder, p_bblock, src_er);
        OpRIA b = op_ria_from_imm(ir_zero_imm);

        IR_Reg cmp_reg = IR_next_reg(builder);
        cmp_instr = IR_emit_instr_int_cmp(builder, *p_bblock, type, cond, cmp_reg, a, b);
    }
    else if (type->kind == TYPE_FLOAT) {
        *p_bblock = IR_expr_result_to_reg(builder, *p_bblock, src_er);

        FloatKind fkind = type->as_float.kind;
        IR_Reg a = src_er->reg;
        OpRA b = {.is_addr = true, .addr = IR_get_zero_flt_addr(builder, fkind)};

        IR_Reg cmp_reg = IR_next_reg(builder);
        cmp_instr = IR_emit_instr_flt_cmp(builder, *p_bblock, fkind, cond, cmp_reg, a, b);
    }
    else {
        NIBBLE_FATAL_EXIT("IR_cmp_to_zero(): Unexpected expression type '%s'", type_name(type));
    }

    return cmp_instr;
}

static void IR_zero_memory(IR_ProcBuilder* builder, BBlock* bblock, MemAddr* addr, size_t size)
{
    if (size > (PTR_SIZE << 2)) {
        OpRI v = {.is_imm = true, .imm.as_int._u64 = 0};
        OpRI s = {.is_imm = true, .imm.as_int._u64 = size};
        IR_emit_instr_memset(builder, bblock, *addr, v, s);
        return;
    }

    Type* chunk_types[] = {
        [1] = builtin_types[BUILTIN_TYPE_U8].type,
        [2] = builtin_types[BUILTIN_TYPE_U16].type,
        [4] = builtin_types[BUILTIN_TYPE_U32].type,
        [8] = builtin_types[BUILTIN_TYPE_U64].type,
    };

    MemAddr chunk_addr = *addr;
    size_t chunk_size = PTR_SIZE;
    size_t rem_bytes = size;

    while (chunk_size) {
        size_t num_chunks = rem_bytes / chunk_size;

        for (size_t i = 0; i < num_chunks; i++) {
            Type* t = chunk_types[chunk_size]; // TODO: store instruction should NOT need a type
            assert(t);

            IR_emit_instr_store(builder, bblock, t, chunk_addr, op_ri_from_imm(ir_zero_imm));

            chunk_addr.disp += chunk_size;
        }

        rem_bytes = rem_bytes % chunk_size;
        chunk_size = chunk_size >> 1;
    }

    assert((chunk_addr.disp - addr->disp) == size);
}

static IR_TmpObj* IR_get_tmp_obj(IR_ProcBuilder* builder, IR_TmpObjList* obj_list, size_t size, size_t align)
{
    IR_TmpObj* tmp_obj;

    // Try to get one from the free list. Otherwise, allocate one.
    if (builder->tmp_obj_freelist) {
        tmp_obj = builder->tmp_obj_freelist;
        builder->tmp_obj_freelist = tmp_obj->next;
    }
    else {
        tmp_obj = alloc_type(builder->tmp_arena, IR_TmpObj, false);
    }

    // Initialize tmp obj.
    tmp_obj->size = size;
    tmp_obj->align = align;
    tmp_obj->mem_obj = alloc_type(builder->arena, MemObj, true);
    tmp_obj->next = NULL;

    // Add obj to the end of the list.
    if (obj_list->last) {
        obj_list->last->next = tmp_obj;
    }
    else {
        obj_list->first = tmp_obj;
    }

    obj_list->last = tmp_obj;

    return tmp_obj;
}

static BBlock* IR_emit_assign(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* lhs, IR_ExprResult* rhs);

static BBlock* IR_init_array_slice(IR_ProcBuilder* builder, BBlock* bblock, MemAddr* slice_addr, Type* slice_type,
                                   IR_ExprResult* array_er)
{
    assert(array_er->type->kind == TYPE_ARRAY);

    BBlock* curr_bb = bblock;

    TypeAggregateField* length_field = get_type_struct_field(slice_type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_LENGTH]);
    IR_ExprResult length_val_er = {.kind = IR_EXPR_RESULT_IMM,
                                   .type = length_field->type,
                                   .imm.as_int._u64 = array_er->type->as_array.len};

    IR_ExprResult length_field_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = length_field->type, .addr = *slice_addr};
    length_field_er.addr.disp += length_field->offset;

    TypeAggregateField* data_field = get_type_struct_field(slice_type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA]);
    IR_ExprResult data_field_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = data_field->type, .addr = *slice_addr};
    data_field_er.addr.disp += data_field->offset;

    MemAddr arr_addr = {0};
    IR_get_object_addr(builder, curr_bb, &arr_addr, array_er);

    IR_ExprResult data_val_er = {.kind = IR_EXPR_RESULT_MEM_ADDR, .type = data_field->type, .addr = arr_addr};

    curr_bb = IR_emit_assign(builder, curr_bb, &length_field_er, &length_val_er);
    curr_bb = IR_emit_assign(builder, curr_bb, &data_field_er, &data_val_er);

    return curr_bb;
}

static BBlock* IR_emit_assign(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* lhs, IR_ExprResult* rhs)
{
    BBlock* curr_bb = bblock;
    MemAddr dst_addr;
    IR_get_object_addr(builder, curr_bb, &dst_addr, lhs);

    assert(lhs->type == rhs->type);

    if (rhs->kind == IR_EXPR_RESULT_IMM) {
        IR_emit_instr_store(builder, curr_bb, lhs->type, dst_addr, op_ri_from_imm(rhs->imm));
    }
    else if ((rhs->kind == IR_EXPR_RESULT_TMP_OBJ) && (lhs->kind == IR_EXPR_RESULT_TMP_OBJ)) {
        MemObj* r_mem_obj = rhs->tmp_obj->mem_obj;

        assert(r_mem_obj->kind == MEM_OBJ_NONE);

        // Alias tmp objects. rhs will refer to lhs.
        r_mem_obj->kind = MEM_OBJ_ALIAS;
        r_mem_obj->alias = lhs->tmp_obj->mem_obj;
    }
    else if ((rhs->kind == IR_EXPR_RESULT_TMP_OBJ) && (lhs->kind == IR_EXPR_RESULT_VAR)) {
        MemObj* mem_obj = rhs->tmp_obj->mem_obj;

        assert(mem_obj->kind == MEM_OBJ_NONE);

        // Elide copy by replacing temporary object with the variable to which it would otherwise be copied.
        mem_obj->kind = MEM_OBJ_SYM;
        mem_obj->sym = lhs->sym;
    }
    else if ((rhs->kind == IR_EXPR_RESULT_TMP_OBJ) && (lhs->kind == IR_EXPR_RESULT_DEREF_ADDR)) {
        MemObj* mem_obj = rhs->tmp_obj->mem_obj;

        assert(mem_obj->kind == MEM_OBJ_NONE);

        // Elide copy by replacing temporary object with the address to which it would otherwise be copied.
        mem_obj->kind = MEM_OBJ_ADDR;
        mem_obj->addr = lhs->addr;
    }
    else if (IR_type_fits_in_reg(rhs->type) && IS_POW2(rhs->type->size)) {
        OpRI a = IR_expr_result_to_op_ri(builder, &curr_bb, rhs);
        IR_emit_instr_store(builder, curr_bb, lhs->type, dst_addr, a);
    }
    else {
        MemAddr src_addr;
        OpRI size = {.is_imm = true, .imm.as_int._u64 = lhs->type->size};

        IR_get_object_addr(builder, curr_bb, &src_addr, rhs);
        IR_emit_instr_memcpy(builder, curr_bb, dst_addr, src_addr, size);
    }

    return curr_bb;
}

static void IR_push_scope(IR_ProcBuilder* builder, Scope* scope)
{
    builder->curr_scope = scope;
}

static void IR_pop_scope(IR_ProcBuilder* builder)
{
    builder->curr_scope = builder->curr_scope->parent;
}

static void IR_free_deferred_tmp_obj_list(IR_ProcBuilder* builder, IR_TmpObjList* obj_list)
{
    if (!obj_list->last) {
        return;
    }

    // Add entire list to the free list.
    obj_list->last->next = builder->tmp_obj_freelist;
    builder->tmp_obj_freelist = obj_list->first;

    // Clear out list pointers.
    obj_list->first = obj_list->last = NULL;
}

static AnonObj* IR_alloc_tmp_anon_obj(IR_ProcBuilder* builder, size_t size, size_t align)
{
    AnonObj* obj;
    Symbol* proc_sym = builder->curr_proc;
    List* head = &proc_sym->as_proc.tmp_objs;
    List* next = builder->curr_tmp_obj->next;

    if (next == head) {
        // NOTE: Use a negative ID for temporary anonymous objects. This ID is used only for debugging purposes.
        // May consider using a hierarchical ID.
        obj = add_anon_obj(builder->arena, head, -proc_sym->as_proc.num_tmp_objs - 1, size, align);

        proc_sym->as_proc.num_tmp_objs += 1;
        builder->curr_tmp_obj = head->prev;
    }
    else {
        obj = list_entry(next, AnonObj, lnode);

        // Keep the largest size and alignment values when reusing the same memory.
        if (obj->size < size) {
            obj->size = size;
        }

        if (obj->align < align) {
            obj->align = align;
        }

        builder->curr_tmp_obj = next;
    }

    return obj;
}

static void IR_reset_proc_tmp_obj_iterator(IR_ProcBuilder* builder)
{
    // Reset iterator to the proc's first tmp anonymous object.
    Symbol* proc_sym = builder->curr_proc;

    builder->curr_tmp_obj = &proc_sym->as_proc.tmp_objs;
}

static void IR_process_deferred_tmp_objs(IR_ProcBuilder* builder, IR_TmpObjList* obj_list)
{
    // Make any remaining deferred objects into temporary anonymous objects.
    for (IR_TmpObj* it = obj_list->first; it; it = it->next) {
        MemObj* mem_obj = it->mem_obj;

        assert(mem_obj);

        if (mem_obj->kind == MEM_OBJ_NONE) {
            mem_obj->kind = MEM_OBJ_ANON_OBJ;
            mem_obj->anon_obj = IR_alloc_tmp_anon_obj(builder, it->size, it->align);
        }
    }

    IR_free_deferred_tmp_obj_list(builder, obj_list);
}

//////////////////////////////////////////////////////
//
//         Walk AST and emit IR instructions
//
//////////////////////////////////////////////////////

static BBlock* IR_emit_stmt(IR_ProcBuilder* builder, BBlock* bblock, Stmt* stmt, IR_UJmpList* break_ujmps, IR_UJmpList* cont_ujmps);
static BBlock* IR_emit_expr(IR_ProcBuilder* builder, BBlock* bblock, Expr* expr, IR_ExprResult* dst, IR_TmpObjList* tmp_obj_list);

static void IR_emit_expr_ident(IR_ProcBuilder* builder, ExprIdent* eident, IR_ExprResult* dst)
{
    List* head = &eident->ns_ident.idents;
    List* it = head->next;

    IdentNode* inode = list_entry(it, IdentNode, lnode);
    Symbol* sym = lookup_symbol(builder->curr_scope, inode->ident);
    it = it->next;

    // Keep looking up identifiers through module namespaces.
    while (it != head) {
        assert(sym->kind == SYMBOL_MODULE);

        inode = list_entry(it, IdentNode, lnode);

        StmtImport* stmt = (StmtImport*)sym->as_mod.stmt;
        Identifier* sym_name = get_import_sym_name(stmt, inode->ident);

        sym = module_get_export_sym(sym->as_mod.mod, sym_name);
        it = it->next;
    }

    assert(sym);
    IR_expr_result_from_sym(dst, sym);
}

static BBlock* IR_emit_ptr_int_add(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* dst, IR_ExprResult* ptr_er,
                                   IR_ExprResult* int_er, bool add)
{
    BBlock* curr_bb = bblock;
    u64 base_size = ptr_er->type->as_ptr.base->size;

    if (int_er->kind == IR_EXPR_RESULT_IMM) {
        IR_ptr_to_mem_expr_result(builder, curr_bb, ptr_er);

        if (add)
            ptr_er->addr.disp += base_size * int_er->imm.as_int._u64;
        else
            ptr_er->addr.disp -= base_size * int_er->imm.as_int._u64;
    }
    else {
        // If ptr is already using a scale that differs from the pointer's base size, then load the entire address into a register
        // first.
        if ((ptr_er->kind == IR_EXPR_RESULT_MEM_ADDR) && ptr_er->addr.scale && (ptr_er->addr.scale != base_size)) {
            IR_execute_lea(builder, curr_bb, ptr_er);
        }

        IR_ptr_to_mem_expr_result(builder, curr_bb, ptr_er);

        if (ptr_er->addr.scale) {
            curr_bb = IR_expr_result_to_reg(builder, curr_bb, int_er);

            IR_Reg r = IR_next_reg(builder);
            OpRIA a = op_ria_from_reg(ptr_er->addr.index_reg);
            OpRIA b = op_ria_from_reg(int_er->reg);

            if (add)
                IR_emit_instr_int_add(builder, curr_bb, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);
            else
                IR_emit_instr_int_sub(builder, curr_bb, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);

            ptr_er->addr.index_reg = r;
        }
        else {
            curr_bb = IR_expr_result_to_reg(builder, curr_bb, int_er);

            if (!add) {
                IR_Reg a = int_er->reg;

                int_er->reg = IR_next_reg(builder);
                IR_emit_instr_neg(builder, curr_bb, int_er->type, int_er->reg, a);
            }

            ptr_er->addr.scale = base_size;
            ptr_er->addr.index_reg = int_er->reg;
        }
    }

    *dst = *ptr_er;

    return curr_bb;
}

static BBlock* IR_emit_binary_cmp(IR_ProcBuilder* builder, BBlock* bblock, ConditionKind cond_kind, Type* dst_type,
                                  IR_ExprResult* dst_er, IR_ExprResult* left_er, IR_ExprResult* right_er)
{
    assert(left_er->type == right_er->type);
    BBlock* curr_bb = bblock;

    IR_Reg cmp_reg = IR_REG_COUNT;
    IR_Instr* cmp_instr = NULL;

    if (type_is_int_scalar(left_er->type)) {
        OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
        OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);

        cmp_reg = IR_next_reg(builder);
        cmp_instr = IR_emit_instr_int_cmp(builder, curr_bb, left_er->type, cond_kind, cmp_reg, a, b);
    }
    else if (left_er->type->kind == TYPE_FLOAT) {
        curr_bb = IR_expr_result_to_reg(builder, curr_bb, left_er);

        IR_Reg a = left_er->reg;
        OpRA b = IR_expr_result_to_op_ra(builder, &curr_bb, right_er);
        FloatKind fkind = left_er->type->as_float.kind;

        cmp_reg = IR_next_reg(builder);
        cmp_instr = IR_emit_instr_flt_cmp(builder, curr_bb, fkind, cond_kind, cmp_reg, a, b);
    }
    else {
        NIBBLE_FATAL_EXIT("IR_emit_binary_cmp(): Unexpected operand types '%s'", type_name(left_er->type));
    }

    dst_er->type = dst_type;
    dst_er->kind = IR_EXPR_RESULT_DEFERRED_CMP;
    dst_er->cmp.final_jmp.cmp = cmp_instr;
    dst_er->cmp.final_jmp.result = true;
    dst_er->cmp.final_jmp.jmp = NULL;
    dst_er->cmp.first_sc_jmp = NULL;
    dst_er->cmp.last_sc_jmp = NULL;

    return curr_bb;
}

static BBlock* IR_emit_short_circuit_cmp(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* dst_er, ExprBinary* expr,
                                         IR_TmpObjList* tmp_obj_list)
{
    //
    // NOTE: This procedure will create a deferred comparison containing an array of short-circuit jumps and one final
    // jump. If the left and right subexpressions are themselves deferred comparisons, then they will be merged into
    // this parent expression's deferred comparison. Otherwise, subexpressions that are not deferred comparisons will be
    // compared to zero and converted to either a short-circuit jump (left subexpression) or a final jump (right
    // subexpression).
    //

    dst_er->kind = IR_EXPR_RESULT_DEFERRED_CMP;
    dst_er->type = expr->super.type;

    IR_ExprResult left_er = {0};
    IR_ExprResult right_er = {0};

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
    BBlock* left_end_bb = IR_emit_expr(builder, bblock, expr->left, &left_er, tmp_obj_list);
    BBlock* right_bb;

    // If the left subexpression is a deferred comparison, merge into this deferred comparison result.
    //
    // Short-circuit jumps from the left subexpression with the same "short-circuit value" are kept as-is.
    //
    // Short-circuit jumps from the left subexpression with the opposite "short-circuit value" are patched
    // with the current instruction index as the jump target and removed. This ensures that short-circuit jumps
    // with the opposite "short-circuit value" are compared to the right subexpression.
    //
    // The left subexpression's final jump is added as a short-circuit jump.
    //
    assert(type_is_bool(left_er.type));

    if (left_er.kind == IR_EXPR_RESULT_DEFERRED_CMP) {
        // Copy list of short-circuit jumps.
        dst_er->cmp.first_sc_jmp = left_er.cmp.first_sc_jmp;
        dst_er->cmp.last_sc_jmp = left_er.cmp.last_sc_jmp;

        // Convert left expression's final jmp to a short-circuit jmp.
        IR_DeferredJmpcc j;

        right_bb = IR_copy_sc_jmp(builder, left_end_bb, &j, &left_er.cmp.final_jmp, short_circuit_val);
        IR_new_deferred_sc_jmp(builder, &dst_er->cmp, j.cmp, j.result, j.jmp);

        // Patch and remove short-circuit jumps with the opposite "short-circuit value".
        IR_DeferredJmpcc* it = dst_er->cmp.first_sc_jmp;
        IR_DeferredJmpcc* prev_it = NULL;

        while (it) {
            IR_DeferredJmpcc* next_it = it->next;

            if (it->result != short_circuit_val) {
                IR_patch_jmp_target(it->jmp, right_bb);
                IR_del_deferred_sc_jmp(builder, &dst_er->cmp, prev_it, it);
            }

            it = next_it;
            prev_it = it;
        }
    }
    // The left subexpression is some computation (not a deferred comparison). Compare the left subexpression to zero
    // and create a short-circuit jmp.
    else {
        left_end_bb = IR_expr_result_to_reg(builder, left_end_bb, &left_er);

        IR_Instr* cmp_instr = IR_cmp_to_zero(builder, &left_end_bb, &left_er, short_circuit_cond);
        IR_Reg cmp_reg = cmp_instr->kind == IR_InstrIntCmp_KIND ? ((IR_InstrIntCmp*)cmp_instr)->r : ((IR_InstrFltCmp*)cmp_instr)->r;

        right_bb = IR_alloc_bblock(builder);
        IR_Instr* jmp_instr = IR_emit_instr_cond_jmp(builder, left_end_bb, NULL, right_bb, cmp_reg);

        IR_new_deferred_sc_jmp(builder, &dst_er->cmp, cmp_instr, short_circuit_val, jmp_instr);
    }

    // Emit instructions for the right expression.
    BBlock* right_end_bb = IR_emit_expr(builder, right_bb, expr->right, &right_er, tmp_obj_list);
    BBlock* last_bb;

    // If the right subexpression is a deferred comparison, merge into this deferred comparison result.
    // The right subexpression's short-circuit jumps are kept as-is.
    // The right subexpression's final jump is converted to a final jump to the "false" control path.
    assert(type_is_bool(right_er.type));

    if (right_er.kind == IR_EXPR_RESULT_DEFERRED_CMP) {
        // Merge lists of short-circuit jumps.
        IR_mov_deferred_sc_jmp_list(&dst_er->cmp, &right_er.cmp);

        // Convert the right expression's final jmp into a final jmp to the "false" path.
        last_bb = IR_copy_sc_jmp(builder, right_end_bb, &dst_er->cmp.final_jmp, &right_er.cmp.final_jmp, false);
    }
    // The right subexpression is some computation (not a deferred comparison). Compare the right subexpression to zero
    // and create a final jump.
    else {
        right_end_bb = IR_expr_result_to_reg(builder, right_end_bb, &right_er);

        IR_Instr* cmp_instr = IR_cmp_to_zero(builder, &right_end_bb, &right_er, COND_EQ);
        IR_Reg cmp_reg = cmp_instr->kind == IR_InstrIntCmp_KIND ? ((IR_InstrIntCmp*)cmp_instr)->r : ((IR_InstrFltCmp*)cmp_instr)->r;

        last_bb = IR_alloc_bblock(builder);
        IR_Instr* jmp_instr = IR_emit_instr_cond_jmp(builder, right_end_bb, NULL, last_bb, cmp_reg);

        dst_er->cmp.final_jmp.result = false;
        dst_er->cmp.final_jmp.jmp = jmp_instr;
        dst_er->cmp.final_jmp.cmp = cmp_instr;
    }

    return last_bb;
}

typedef BBlock* IR_EmitBinOpProc(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                                 IR_ExprResult* dst_er, Type* dst_type);

static BBlock* IR_emit_op_add(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;
    TypeKind left_kind = left_er->type->kind;
    TypeKind right_kind = right_er->type->kind;

    if (left_kind == TYPE_PTR) {
        curr_bb = IR_emit_ptr_int_add(builder, curr_bb, dst_er, left_er, right_er, true);
    }
    else if (right_kind == TYPE_PTR) {
        curr_bb = IR_emit_ptr_int_add(builder, curr_bb, dst_er, right_er, left_er, true);
    }
    else if (left_kind == TYPE_FLOAT) {
        assert(left_er->type == right_er->type);
        OpRA a = IR_expr_result_to_op_ra(builder, &curr_bb, left_er);
        OpRA b = IR_expr_result_to_op_ra(builder, &curr_bb, right_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_flt_add(builder, curr_bb, dst_type->as_float.kind, dst_reg, a, b);

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->type = dst_type;
        dst_er->reg = dst_reg;
    }
    else {
        assert(type_is_integer_like(left_er->type));
        assert(left_er->type == right_er->type);
        OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
        OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_int_add(builder, curr_bb, dst_type, dst_reg, a, b);

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->type = dst_type;
        dst_er->reg = dst_reg;
    }

    return curr_bb;
}

static BBlock* IR_emit_op_sub(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;
    TypeKind left_kind = left_er->type->kind;
    TypeKind right_kind = right_er->type->kind;
    bool left_is_ptr = left_kind == TYPE_PTR;
    bool right_is_ptr = right_kind == TYPE_PTR;

    // ptr - int => ptr
    if (left_is_ptr && !right_is_ptr) {
        curr_bb = IR_emit_ptr_int_add(builder, curr_bb, dst_er, left_er, right_er, false);
    }
    // ptr - ptr => s64
    else if (left_is_ptr && right_is_ptr) {
        u64 base_size = left_er->type->as_ptr.base->size;
        u32 base_size_log2 = (u32)clp2(base_size);

        OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
        OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_int_sub(builder, curr_bb, dst_type, dst_reg, a, b);

        if (base_size_log2 > 0) {
            // Shift result of subtraction by the shift amount.
            OpRIA unshifted = op_ria_from_reg(dst_reg);
            OpRIA shift_arg = op_ria_from_imm((Scalar){.as_int._u32 = base_size_log2});
            dst_reg = IR_next_reg(builder);
            IR_emit_instr_sar(builder, curr_bb, dst_type, dst_reg, unshifted, shift_arg);
        }

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->type = dst_type;
        dst_er->reg = dst_reg;
    }
    // float - float = float
    else if (left_kind == TYPE_FLOAT) {
        assert(left_er->type == right_er->type);
        OpRA a = IR_expr_result_to_op_ra(builder, &curr_bb, left_er);
        OpRA b = IR_expr_result_to_op_ra(builder, &curr_bb, right_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_flt_sub(builder, curr_bb, dst_type->as_float.kind, dst_reg, a, b);

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->type = dst_type;
        dst_er->reg = dst_reg;
    }
    // int - int => int
    else {
        assert(type_is_integer_like(left_er->type));
        assert(left_er->type == right_er->type);
        OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
        OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_int_sub(builder, curr_bb, dst_type, dst_reg, a, b);

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->type = dst_type;
        dst_er->reg = dst_reg;
    }

    return curr_bb;
}

static BBlock* IR_emit_op_mul(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;
    IR_Reg dst_reg = IR_next_reg(builder);

    if (type_is_integer_like(dst_type)) {
        OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
        OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);

        // TODO: Emit a shift instruction if one of the expr_results is a power-of-two immediate.
        IR_emit_instr_int_mul(builder, curr_bb, dst_type, dst_reg, a, b);
    }
    else if (dst_type->kind == TYPE_FLOAT) {
        OpRA a = IR_expr_result_to_op_ra(builder, &curr_bb, left_er);
        OpRA b = IR_expr_result_to_op_ra(builder, &curr_bb, right_er);

        IR_emit_instr_flt_mul(builder, curr_bb, dst_type->as_float.kind, dst_reg, a, b);
    }
    else {
        NIBBLE_FATAL_EXIT("IR_emit_op_mul() unexpected dst type (%s)", type_name(dst_type));
    }

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_op_div(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;
    IR_Reg dst_reg = IR_next_reg(builder);

    if (type_is_integer_like(dst_type)) {
        OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
        OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);

        // TODO: Emit a shift instruction if the second expr_result is a power-of-two immediate.
        IR_emit_instr_int_div(builder, curr_bb, dst_type, dst_reg, a, b);
    }
    else if (dst_type->kind == TYPE_FLOAT) {
        OpRA a = IR_expr_result_to_op_ra(builder, &curr_bb, left_er);
        OpRA b = IR_expr_result_to_op_ra(builder, &curr_bb, right_er);

        IR_emit_instr_flt_div(builder, curr_bb, dst_type->as_float.kind, dst_reg, a, b);
    }
    else {
        NIBBLE_FATAL_EXIT("IR_emit_op_div() unexpected dst_type (%s)", type_name(dst_type));
    }

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_op_mod(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;

    OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
    OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
    IR_Reg dst_reg = IR_next_reg(builder);

    // TODO: Emit a masking instruction if the second expr_result is a power-of-two immediate.
    IR_emit_instr_mod(builder, curr_bb, dst_type, dst_reg, a, b);

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_op_and(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;

    OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
    OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_and(builder, curr_bb, dst_type, dst_reg, a, b);

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_op_or(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                             IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;

    OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
    OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_or(builder, curr_bb, dst_type, dst_reg, a, b);

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_op_xor(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;

    OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
    OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_xor(builder, curr_bb, dst_type, dst_reg, a, b);

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_op_sar(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;

    OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
    OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_sar(builder, curr_bb, dst_type, dst_reg, a, b);

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_op_shl(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* left_er, IR_ExprResult* right_er,
                              IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;

    OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, left_er);
    OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, right_er);
    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_shl(builder, curr_bb, dst_type, dst_reg, a, b);

    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->type = dst_type;
    dst_er->reg = dst_reg;

    return curr_bb;
}

static BBlock* IR_emit_expr_binary(IR_ProcBuilder* builder, BBlock* bblock, ExprBinary* expr, IR_ExprResult* dst,
                                   IR_TmpObjList* tmp_obj_list)
{
    if (expr->op == TKN_LOGIC_AND || expr->op == TKN_LOGIC_OR) {
        return IR_emit_short_circuit_cmp(builder, bblock, dst, expr, tmp_obj_list);
    }

    Type* result_type = expr->super.type;
    IR_ExprResult left = {0};
    IR_ExprResult right = {0};

    BBlock* curr_bb = IR_emit_expr(builder, bblock, expr->left, &left, tmp_obj_list);
    curr_bb = IR_emit_expr(builder, curr_bb, expr->right, &right, tmp_obj_list);

    switch (expr->op) {
    case TKN_PLUS: {
        curr_bb = IR_emit_op_add(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_MINUS: {
        curr_bb = IR_emit_op_sub(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_ASTERISK: {
        curr_bb = IR_emit_op_mul(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_DIV: {
        curr_bb = IR_emit_op_div(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_MOD: {
        curr_bb = IR_emit_op_mod(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_DIVMOD: {
        // Allocate a temporary array object to hold both the quotient and remainder.
        IR_TmpObj* result_obj = IR_get_tmp_obj(builder, tmp_obj_list, result_type->size, result_type->align);
        MemAddr result_addr = IR_tmp_obj_as_addr(result_obj);

        IR_ExprResult quot_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = left.type, .addr = result_addr};
        IR_ExprResult rem_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = left.type, .addr = result_addr};
        rem_er.addr.disp += left.type->size;

        IR_ExprResult quot_val = {.type = left.type};
        IR_ExprResult rem_val = {.type = left.type};

        if (left.kind == IR_EXPR_RESULT_IMM && right.kind == IR_EXPR_RESULT_IMM) {
            quot_val.kind = IR_EXPR_RESULT_IMM;
            rem_val.kind = IR_EXPR_RESULT_IMM;

            if (type_is_signed(left.type)) {
                quot_val.imm.as_int._s64 = left.imm.as_int._s64 / right.imm.as_int._s64;
                rem_val.imm.as_int._s64 = left.imm.as_int._s64 % right.imm.as_int._s64;
            }
            else {
                quot_val.imm.as_int._u64 = left.imm.as_int._u64 / right.imm.as_int._u64;
                rem_val.imm.as_int._u64 = left.imm.as_int._u64 % right.imm.as_int._u64;
            }
        }
        else {
            OpRIA a = IR_expr_result_to_op_ria(builder, &curr_bb, &left);
            OpRIA b = IR_expr_result_to_op_ria(builder, &curr_bb, &right);
            IR_Reg quot_reg = IR_next_reg(builder);
            IR_Reg rem_reg = IR_next_reg(builder);

            // TODO: Emit shift + masking instructions if the second expr_result is a power-of-two immediate.
            IR_emit_instr_divmod(builder, curr_bb, left.type, quot_reg, rem_reg, a, b);

            quot_val.kind = IR_EXPR_RESULT_REG;
            quot_val.reg = quot_reg;

            rem_val.kind = IR_EXPR_RESULT_REG;
            rem_val.reg = rem_reg;
        }

        // Copy quotient and remainer into the object.
        curr_bb = IR_emit_assign(builder, curr_bb, &quot_er, &quot_val);
        curr_bb = IR_emit_assign(builder, curr_bb, &rem_er, &rem_val);

        dst->kind = IR_EXPR_RESULT_TMP_OBJ;
        dst->type = result_type;
        dst->tmp_obj = result_obj;
        break;
    }
    case TKN_RSHIFT: {
        curr_bb = IR_emit_op_sar(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_LSHIFT: {
        curr_bb = IR_emit_op_shl(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_AND: {
        curr_bb = IR_emit_op_and(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_OR: {
        curr_bb = IR_emit_op_or(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_CARET: {
        curr_bb = IR_emit_op_xor(builder, curr_bb, &left, &right, dst, result_type);
        break;
    }
    case TKN_EQ: {
        curr_bb = IR_emit_binary_cmp(builder, curr_bb, COND_EQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_NOTEQ: {
        curr_bb = IR_emit_binary_cmp(builder, curr_bb, COND_NEQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_LT: {
        ConditionKind cond_kind = left.type->kind == TYPE_FLOAT || type_is_signed(left.type) ? COND_S_LT : COND_U_LT;

        curr_bb = IR_emit_binary_cmp(builder, curr_bb, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_LTEQ: {
        ConditionKind cond_kind = left.type->kind == TYPE_FLOAT || type_is_signed(left.type) ? COND_S_LTEQ : COND_U_LTEQ;

        curr_bb = IR_emit_binary_cmp(builder, curr_bb, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GT: {
        ConditionKind cond_kind = left.type->kind == TYPE_FLOAT || type_is_signed(left.type) ? COND_S_GT : COND_U_GT;

        curr_bb = IR_emit_binary_cmp(builder, curr_bb, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GTEQ: {
        ConditionKind cond_kind = left.type->kind == TYPE_FLOAT || type_is_signed(left.type) ? COND_S_GTEQ : COND_U_GTEQ;

        curr_bb = IR_emit_binary_cmp(builder, curr_bb, cond_kind, result_type, dst, &left, &right);
        break;
    }
    default:
        assert(0);
        break;
    }

    return curr_bb;
}

static BBlock* IR_process_cfg_cond(IR_ProcBuilder* builder, Expr* expr, BBlock* hdr_bb, bool jmp_result, BBlock* jmp_bb,
                                   IR_TmpObjList* tmp_obj_list);

static BBlock* IR_emit_expr_ternary(IR_ProcBuilder* builder, BBlock* bblock, ExprTernary* expr, IR_ExprResult* dst,
                                    IR_TmpObjList* tmp_obj_list)
{
    // If the condition is a compile-time constant, do not generate an unnecessary branch.
    if (expr->cond->is_constexpr) {
        assert(expr->cond->is_imm);
        Expr* e = expr->cond->imm.as_int._bool ? expr->then_expr : expr->else_expr;

        return IR_emit_expr(builder, bblock, e, dst, tmp_obj_list);
    }

    Type* result_type = expr->super.type;
    assert(result_type == expr->then_expr->type);
    assert(result_type == expr->else_expr->type);
    bool obj_like = type_is_obj_like(result_type);

    BBlock* false_bb = IR_alloc_bblock(builder);
    BBlock* last_bb = IR_alloc_bblock(builder);
    BBlock* false_tgt = false_bb;

    // Process condition.
    BBlock* true_bb = IR_process_cfg_cond(builder, expr->cond, bblock, false, false_tgt, tmp_obj_list);

    // Emit instructions for then-expression
    IR_ExprResult then_er = {0};
    BBlock* true_end_bb = IR_emit_expr(builder, true_bb, expr->then_expr, &then_er, tmp_obj_list);

    if (obj_like) {
        IR_TmpObj* tmp_obj = IR_get_tmp_obj(builder, tmp_obj_list, result_type->size, result_type->align);
        IR_ExprResult tmp_obj_er = {.kind = IR_EXPR_RESULT_TMP_OBJ, .type = result_type, .tmp_obj = tmp_obj};

        true_end_bb = IR_emit_assign(builder, true_end_bb, &tmp_obj_er, &then_er);
        then_er = tmp_obj_er;
    }
    else {
        true_end_bb = IR_expr_result_to_reg(builder, true_end_bb, &then_er);
    }

    // Skip 'else' expression
    assert(true_end_bb);
    IR_emit_instr_jmp(builder, true_end_bb, last_bb);

    // Emit instructions for else-expression.
    IR_ExprResult else_er = {0};
    BBlock* false_end_bb = IR_emit_expr(builder, false_bb, expr->else_expr, &else_er, tmp_obj_list);

    if (obj_like) {
        assert(then_er.kind == IR_EXPR_RESULT_TMP_OBJ);
        IR_TmpObj* tmp_obj = then_er.tmp_obj; // Reuse the same temporary object!
        IR_ExprResult tmp_obj_er = {.kind = IR_EXPR_RESULT_TMP_OBJ, .type = result_type, .tmp_obj = tmp_obj};

        false_end_bb = IR_emit_assign(builder, false_end_bb, &tmp_obj_er, &else_er);
        else_er = tmp_obj_er;
    }
    else {
        false_end_bb = IR_expr_result_to_reg(builder, false_end_bb, &else_er);
    }

    // Jump from else bblock to last bblock.
    assert(false_end_bb);
    IR_emit_instr_jmp(builder, false_end_bb, last_bb);

    if (obj_like) {
        dst->kind = IR_EXPR_RESULT_TMP_OBJ;
        dst->type = result_type;
        dst->tmp_obj = then_er.tmp_obj;
    }
    else {
        dst->kind = IR_EXPR_RESULT_REG;
        dst->type = result_type;
        dst->reg = IR_next_reg(builder);

        PhiArg phi_args[2] = {{.bblock = true_end_bb, .ireg = then_er.reg}, {.bblock = false_end_bb, .ireg = else_er.reg}};
        IR_emit_instr_phi(builder, last_bb, result_type, dst->reg, 2, phi_args);
    }

    return last_bb;
}

static BBlock* IR_emit_expr_unary(IR_ProcBuilder* builder, BBlock* bblock, ExprUnary* expr, IR_ExprResult* dst,
                                  IR_TmpObjList* tmp_obj_list)
{
    Type* result_type = expr->super.type;
    BBlock* curr_bb = bblock;

    switch (expr->op) {
    case TKN_PLUS: {
        curr_bb = IR_emit_expr(builder, curr_bb, expr->expr, dst, tmp_obj_list);
        break;
    }
    case TKN_MINUS: // Two's compliment negation.
    {
        IR_ExprResult src;

        curr_bb = IR_emit_expr(builder, curr_bb, expr->expr, &src, tmp_obj_list);
        curr_bb = IR_expr_result_to_reg(builder, curr_bb, &src);

        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_neg(builder, curr_bb, result_type, dst_reg, src.reg);

        dst->kind = IR_EXPR_RESULT_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_NEG: // Bitwise not
    {
        IR_ExprResult src;

        curr_bb = IR_emit_expr(builder, curr_bb, expr->expr, &src, tmp_obj_list);
        curr_bb = IR_expr_result_to_reg(builder, curr_bb, &src);

        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_not(builder, curr_bb, result_type, dst_reg, src.reg);

        dst->kind = IR_EXPR_RESULT_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_NOT: // Logical not
    {
        dst->kind = IR_EXPR_RESULT_DEFERRED_CMP;
        dst->type = result_type;

        IR_ExprResult inner_er = {0};
        curr_bb = IR_emit_expr(builder, curr_bb, expr->expr, &inner_er, tmp_obj_list);

        if (inner_er.kind == IR_EXPR_RESULT_DEFERRED_CMP) {
            // Reverse control paths for all jumps.
            // Ex: if a jmp instruction jumps to the "true" path, make it jump to the "false" path.
            dst->cmp.first_sc_jmp = inner_er.cmp.first_sc_jmp;
            dst->cmp.last_sc_jmp = inner_er.cmp.last_sc_jmp;

            for (IR_DeferredJmpcc* it = dst->cmp.first_sc_jmp; it; it = it->next) {
                it->result = !(it->result);
            }

            dst->cmp.final_jmp = inner_er.cmp.final_jmp;
            dst->cmp.final_jmp.result = !inner_er.cmp.final_jmp.result;
        }
        else {
            IR_Instr* cmp_instr = IR_cmp_to_zero(builder, &curr_bb, &inner_er, COND_EQ);

            dst->cmp.final_jmp.cmp = cmp_instr;
            dst->cmp.final_jmp.result = true;
            dst->cmp.first_sc_jmp = NULL;
            dst->cmp.last_sc_jmp = NULL;
            dst->cmp.final_jmp.jmp = NULL;
        }

        break;
    }
    case TKN_ASTERISK: {
        curr_bb = IR_emit_expr(builder, curr_bb, expr->expr, dst, tmp_obj_list);
        IR_ptr_to_mem_expr_result(builder, curr_bb, dst);

        dst->kind = IR_EXPR_RESULT_DEREF_ADDR;
        dst->type = result_type;
        break;
    }
    case TKN_CARET: // Address-of operator
    {
        IR_ExprResult src;
        curr_bb = IR_emit_expr(builder, curr_bb, expr->expr, &src, tmp_obj_list);

        dst->kind = IR_EXPR_RESULT_MEM_ADDR;
        dst->type = result_type;

        if (src.kind == IR_EXPR_RESULT_DEREF_ADDR) {
            dst->addr = src.addr;
        }
        else {
            assert(src.kind == IR_EXPR_RESULT_VAR);
            dst->addr = IR_sym_as_addr(builder, src.sym);
        }
        break;
    }
    default:
        assert(0);
        break;
    }

    return curr_bb;
}

static BBlock* IR_emit_obj_expr(IR_ProcBuilder* builder, BBlock* bblock, Expr* obj_expr, IR_ExprResult* dst,
                                IR_TmpObjList* tmp_obj_list)
{
    IR_ExprResult obj_er = {0};
    BBlock* curr_bb = IR_emit_expr(builder, bblock, obj_expr, &obj_er, tmp_obj_list);

    dst->kind = IR_EXPR_RESULT_DEREF_ADDR;

    // This pointer points to the actual object.
    if (obj_er.type->kind == TYPE_PTR) {
        dst->type = obj_er.type->as_ptr.base;
        IR_ptr_to_mem_expr_result(builder, curr_bb, &obj_er);
        dst->addr = obj_er.addr;
    }
    // Accessing the object field directly.
    else {
        dst->type = obj_er.type;
        IR_get_object_addr(builder, curr_bb, &dst->addr, &obj_er);
    }

    return curr_bb;
}

static BBlock* IR_emit_expr_field(IR_ProcBuilder* builder, BBlock* bblock, ExprField* expr_field, IR_ExprResult* dst,
                                  IR_TmpObjList* tmp_obj_list)
{
    IR_ExprResult obj_er = {0};
    BBlock* curr_bb = IR_emit_obj_expr(builder, bblock, expr_field->object, &obj_er, tmp_obj_list);

    TypeAggregateField* field = get_type_aggregate_field(obj_er.type, expr_field->field);
    assert(field);

    dst->kind = IR_EXPR_RESULT_DEREF_ADDR;
    dst->type = expr_field->super.type;
    dst->addr = obj_er.addr;

    // Add in the field's byte offset from the beginning of the object.
    dst->addr.disp += (u32)field->offset;

    return curr_bb;
}

static BBlock* IR_emit_expr_field_index(IR_ProcBuilder* builder, BBlock* bblock, ExprFieldIndex* expr, IR_ExprResult* dst,
                                        IR_TmpObjList* tmp_obj_list)
{
    IR_ExprResult obj_er = {0};
    BBlock* curr_bb = IR_emit_obj_expr(builder, bblock, expr->object, &obj_er, tmp_obj_list);

    assert(expr->index->is_imm);

    size_t field_index = expr->index->imm.as_int._u64;
    TypeAggregateBody* type_agg = obj_er.type->kind == TYPE_STRUCT ? &obj_er.type->as_struct.body : &obj_er.type->as_union.body;

    assert(field_index < type_agg->num_fields);

    TypeAggregateField* field = type_agg->fields + field_index;

    dst->kind = IR_EXPR_RESULT_DEREF_ADDR;
    dst->type = expr->super.type;
    dst->addr = obj_er.addr;

    // Add in the field's byte offset from the beginning of the object.
    dst->addr.disp += (u32)field->offset;

    return curr_bb;
}

static BBlock* IR_emit_expr_index(IR_ProcBuilder* builder, BBlock* bblock, ExprIndex* expr_index, IR_ExprResult* dst,
                                  IR_TmpObjList* tmp_obj_list)
{
    IR_ExprResult array_er = {0};
    IR_ExprResult index_er = {0};

    BBlock* curr_bb = IR_emit_expr(builder, bblock, expr_index->array, &array_er, tmp_obj_list);
    curr_bb = IR_emit_expr(builder, curr_bb, expr_index->index, &index_er, tmp_obj_list);

    assert(array_er.type->kind == TYPE_PTR);

    curr_bb = IR_emit_ptr_int_add(builder, curr_bb, dst, &array_er, &index_er, true);

    dst->kind = IR_EXPR_RESULT_DEREF_ADDR;
    dst->type = expr_index->super.type;

    return curr_bb;
}

static BBlock* IR_emit_int_cast(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* src_er, Type* dst_type, IR_ExprResult* dst_er)
{
    // NOTE:
    // This function treats pointers like integers. The IR currently implements "opaque" pointers, so
    // there are no explicit instructions for converting from one ptr type to another, or converting to/from int/ptr.
    assert(src_er->kind != IR_EXPR_RESULT_IMM); // Should be prevented by resolver.

    Type* src_type = src_er->type;
    size_t src_size = src_type->size;
    size_t dst_size = dst_type->size;

    // Converting between pointer types.
    if (dst_type->kind == TYPE_PTR && src_type->kind == TYPE_PTR) {
        IR_ptr_to_mem_expr_result(builder, bblock, src_er);

        dst_er->kind = IR_EXPR_RESULT_MEM_ADDR;
        dst_er->addr = src_er->addr;
        return bblock;
    }

    // Integers (or ptrs) are the same size. This is a NO-OP even if any of the types is a ptr type.
    if (src_size == dst_size) {
        *dst_er = *src_er;
        dst_er->type = dst_type;
        return bblock;
    }

    // We need the src expression to be in a register.
    BBlock* curr_bb = IR_expr_result_to_reg(builder, bblock, src_er);

    IR_Reg dst_reg = IR_REG_COUNT;
    IR_Reg src_reg = src_er->reg;

    // Truncate from larger type to smaller type.
    if (src_size > dst_size) {
        dst_reg = IR_next_reg(builder);
        IR_emit_instr_trunc(builder, curr_bb, dst_type, src_type, dst_reg, src_reg);
    }
    // Extend (sign or zero) src to larger type.
    else {
        dst_reg = IR_next_reg(builder);

        if (type_is_signed(src_type)) {
            IR_emit_instr_sext(builder, curr_bb, dst_type, src_type, dst_reg, src_reg);
        }
        else {
            IR_emit_instr_zext(builder, curr_bb, dst_type, src_type, dst_reg, src_reg);
        }
    }

    if (dst_er->type->kind == TYPE_PTR) {
        dst_er->kind = IR_EXPR_RESULT_MEM_ADDR;
        dst_er->addr.base_kind = MEM_BASE_REG;
        dst_er->addr.base.reg = dst_reg;
        dst_er->addr.index_reg = IR_REG_COUNT;
        dst_er->addr.scale = 0;
        dst_er->addr.disp = 0;
    }
    else {
        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->reg = dst_reg;
    }

    return curr_bb;
}

static BBlock* IR_emit_op_cast(IR_ProcBuilder* builder, BBlock* bblock, IR_TmpObjList* tmp_obj_list, IR_ExprResult* src_er,
                               IR_ExprResult* dst_er, Type* dst_type)
{
    BBlock* curr_bb = bblock;
    dst_er->type = dst_type;

    if (src_er->type == dst_er->type) {
        *dst_er = *src_er; // The source expression is already the correct type. This is a no-op.
        return curr_bb;
    }

    if ((src_er->type->kind == TYPE_ARRAY) && (dst_er->type->kind == TYPE_PTR)) {
        dst_er->kind = IR_EXPR_RESULT_MEM_ADDR;

        IR_get_object_addr(builder, curr_bb, &dst_er->addr, src_er);
    }
    else if (type_is_slice(src_er->type) && (dst_er->type->kind == TYPE_PTR)) {
        MemAddr slice_addr = {0};
        IR_get_object_addr(builder, curr_bb, &slice_addr, src_er);

        TypeAggregateField* data_field = get_type_struct_field(src_er->type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA]);
        assert(data_field->type == dst_er->type);

        dst_er->kind = IR_EXPR_RESULT_DEREF_ADDR;
        dst_er->addr = slice_addr;
        dst_er->addr.disp += data_field->offset;

        IR_execute_deref(builder, curr_bb, dst_er);
    }
    else if ((src_er->type->kind == TYPE_ARRAY) && type_is_slice(dst_er->type)) {
        Type* slice_type = dst_er->type;
        IR_TmpObj* slice_obj = IR_get_tmp_obj(builder, tmp_obj_list, slice_type->size, slice_type->align);
        MemAddr slice_addr = IR_tmp_obj_as_addr(slice_obj);

        curr_bb = IR_init_array_slice(builder, curr_bb, &slice_addr, slice_type, src_er);

        dst_er->kind = IR_EXPR_RESULT_TMP_OBJ;
        dst_er->tmp_obj = slice_obj;
    }
    else if (type_is_bool(dst_er->type) && type_is_scalar(src_er->type)) {
        assert(src_er->kind != IR_EXPR_RESULT_DEFERRED_CMP);

        IR_Instr* cmp_instr = IR_cmp_to_zero(builder, &curr_bb, src_er, COND_NEQ);

        // Create a "deferred" comparison that will either be resolved into a boolean value or used in a conditional jump.
        dst_er->kind = IR_EXPR_RESULT_DEFERRED_CMP;
        dst_er->cmp.final_jmp.cmp = cmp_instr;
        dst_er->cmp.final_jmp.result = true;
        dst_er->cmp.final_jmp.jmp = NULL;
        dst_er->cmp.first_sc_jmp = NULL;
        dst_er->cmp.last_sc_jmp = NULL;
    }
    else if (type_is_integer_like(src_er->type) && dst_er->type->kind == TYPE_FLOAT) {
        OpRA s = IR_expr_result_to_op_ra(builder, &curr_bb, src_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        FloatKind dst_kind = dst_er->type->as_float.kind;
        IntegerKind src_kind =
            src_er->type->kind == TYPE_ENUM ? src_er->type->as_enum.base->as_integer.kind : src_er->type->as_integer.kind;

        IR_emit_instr_int2flt(builder, curr_bb, dst_kind, src_kind, dst_reg, s);

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->reg = dst_reg;
    }
    else if ((src_er->type->kind == TYPE_FLOAT) && type_is_integer_like(dst_er->type)) {
        OpRA s = IR_expr_result_to_op_ra(builder, &curr_bb, src_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        IntegerKind dst_kind =
            dst_er->type->kind == TYPE_ENUM ? dst_er->type->as_enum.base->as_integer.kind : dst_er->type->as_integer.kind;
        FloatKind src_kind = src_er->type->as_float.kind;

        IR_emit_instr_flt2int(builder, curr_bb, dst_kind, src_kind, dst_reg, s);

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->reg = dst_reg;
    }
    else if ((src_er->type->kind == TYPE_FLOAT) && (dst_er->type->kind == TYPE_FLOAT)) {
        OpRA s = IR_expr_result_to_op_ra(builder, &curr_bb, src_er);
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_flt2flt(builder, curr_bb, dst_er->type->as_float.kind, src_er->type->as_float.kind, dst_reg, s);

        dst_er->kind = IR_EXPR_RESULT_REG;
        dst_er->reg = dst_reg;
    }
    else if (type_is_scalar(src_er->type) && type_is_scalar(dst_er->type)) {
        curr_bb = IR_emit_int_cast(builder, curr_bb, src_er, dst_type, dst_er);
    }
    else {
        NIBBLE_FATAL_EXIT("IR_emit_op_cast(): Unexpected arg types '%s' and '%s'.", type_name(src_er->type), type_name(dst_er->type));
    }

    return curr_bb;
}

static BBlock* IR_emit_expr_cast(IR_ProcBuilder* builder, BBlock* bblock, ExprCast* expr_cast, IR_ExprResult* dst_er,
                                 IR_TmpObjList* tmp_obj_list)
{
    Type* dst_type = expr_cast->super.type;
    IR_ExprResult src_er = {0};

    BBlock* curr_bb = IR_emit_expr(builder, bblock, expr_cast->expr, &src_er, tmp_obj_list);

    return IR_emit_op_cast(builder, curr_bb, tmp_obj_list, &src_er, dst_er, dst_type);
}

static BBlock* IR_emit_expr_bit_cast(IR_ProcBuilder* builder, BBlock* bblock, ExprBitCast* expr_cast, IR_ExprResult* dst_er,
                                     IR_TmpObjList* tmp_obj_list)
{
    BBlock* curr_bb = IR_emit_expr(builder, bblock, expr_cast->expr, dst_er, tmp_obj_list);

    dst_er->type = expr_cast->super.type;

    return curr_bb;
}

static BBlock* IR_emit_memcpy_call(IR_ProcBuilder* builder, BBlock* bblock, size_t num_args, List* args, IR_TmpObjList* tmp_obj_list)
{
    BBlock* curr_bb = bblock;

    MemAddr dst_addr = {0};
    MemAddr src_addr = {0};
    OpRI size = {0};

    assert(num_args == 3);

    List* it = args->next;
    size_t arg_index = 0;

    while (arg_index < num_args) {
        assert(it != args);

        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);
        IR_ExprResult arg_er = {0};

        curr_bb = IR_emit_expr(builder, curr_bb, arg->expr, &arg_er, tmp_obj_list);

        switch (arg_index) {
        case 0: // dst : ^void
            assert(arg_er.type == type_ptr_void);
            IR_ptr_to_mem_expr_result(builder, curr_bb, &arg_er);
            dst_addr = arg_er.addr;
            break;
        case 1: // src : ^void
            assert(arg_er.type == type_ptr_void);
            IR_ptr_to_mem_expr_result(builder, curr_bb, &arg_er);
            src_addr = arg_er.addr;
            break;
        case 2: // size: usize
            assert(arg_er.type == builtin_types[BUILTIN_TYPE_USIZE].type);
            size = IR_expr_result_to_op_ri(builder, &curr_bb, &arg_er);
            break;
        default:
            assert(0);
            break;
        }

        arg_index += 1;
        it = it->next;
    }

    assert(it == args);

    IR_emit_instr_memcpy(builder, curr_bb, dst_addr, src_addr, size);

    return curr_bb;
}

static BBlock* IR_emit_memset_call(IR_ProcBuilder* builder, BBlock* bblock, size_t num_args, List* args, IR_TmpObjList* tmp_obj_list)
{
    BBlock* curr_bb = bblock;

    MemAddr dst_addr = {0};
    OpRI value = {0};
    OpRI size = {0};

    assert(num_args == 3);

    List* it = args->next;
    size_t arg_index = 0;

    while (arg_index < num_args) {
        assert(it != args);

        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);
        IR_ExprResult arg_er = {0};

        curr_bb = IR_emit_expr(builder, curr_bb, arg->expr, &arg_er, tmp_obj_list);

        switch (arg_index) {
        case 0: // dst : ^void
            assert(arg_er.type == type_ptr_void);
            IR_ptr_to_mem_expr_result(builder, curr_bb, &arg_er);
            dst_addr = arg_er.addr;
            break;
        case 1: // value : uchar
            assert(arg_er.type == builtin_types[BUILTIN_TYPE_UCHAR].type);
            value = IR_expr_result_to_op_ri(builder, &curr_bb, &arg_er);
            break;
        case 2: // size: usize
            assert(arg_er.type == builtin_types[BUILTIN_TYPE_USIZE].type);
            size = IR_expr_result_to_op_ri(builder, &curr_bb, &arg_er);
            break;
        default:
            assert(0);
            break;
        }

        arg_index += 1;
        it = it->next;
    }

    assert(it == args);

    IR_emit_instr_memset(builder, curr_bb, dst_addr, value, size);

    return curr_bb;
}

// TODO: This should really be a builtin nibble macro that wraps the write syscall.
// Ex: macro #writeout(buf: ^char, size: usize) => ssize { return #syscall3(SYS_write, STDOUT, buf, size); }
static BBlock* IR_emit_writeout_call(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* dst_er, size_t num_args, List* args,
                                     IR_TmpObjList* tmp_obj_list)
{
    BBlock* curr_bb = bblock;

    if (num_args != 2) {
        NIBBLE_FATAL_EXIT("#writeout intrinsic must have 2 arguments."); // Internal compiler error.
        return NULL;
    }

    const s32 STDOUT_FD = 1;
    const s64 SYS_write = 1;

    // syscall_nr, STDOUT, buf, size
    OpRIA syscall_nr = {.kind = OP_RIA_IMM, .imm.as_int._u64 = SYS_write};
    OpRIA syscall_args[3] = {[0] = {.kind = OP_RIA_IMM, .imm.as_int._u64 = STDOUT_FD}};

    List* it = args->next;
    size_t arg_index = 1;

    while (arg_index < ARRAY_LEN(syscall_args)) {
        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);
        IR_ExprResult arg_er = {0};

        curr_bb = IR_emit_expr(builder, curr_bb, arg->expr, &arg_er, tmp_obj_list);

        syscall_args[arg_index] = IR_expr_result_to_op_ria(builder, &curr_bb, &arg_er);

        arg_index += 1;
        it = it->next;
    }

    assert(it == args);

    dst_er->type = builtin_types[BUILTIN_TYPE_SSIZE].type;
    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->reg = IR_next_reg(builder);

    IR_emit_instr_syscall(builder, curr_bb, dst_er->reg, syscall_nr, ARRAY_LEN(syscall_args), syscall_args);

    return curr_bb;
}

// TODO: This should really be a builtin nibble macro that wraps the read syscall.
// Ex: macro #readin(buf: ^char, size: usize) => ssize { return #syscall3(SYS_read, STDIN, buf, size); }
static BBlock* IR_emit_readin_call(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* dst_er, size_t num_args, List* args,
                                   IR_TmpObjList* tmp_obj_list)
{
    BBlock* curr_bb = bblock;

    if (num_args != 2) {
        NIBBLE_FATAL_EXIT("#readin intrinsic must have 2 arguments."); // Internal compiler error.
        return NULL;
    }

    const s32 STDIN_FD = 0;
    const s64 SYS_read = 0;

    // syscall_nr, STDIN, buf, size
    OpRIA syscall_nr = {.kind = OP_RIA_IMM, .imm.as_int._u64 = SYS_read};
    OpRIA syscall_args[3] = {[0] = {.kind = OP_RIA_IMM, .imm.as_int._u64 = STDIN_FD}};

    List* it = args->next;
    size_t arg_index = 1;

    while (arg_index < ARRAY_LEN(syscall_args)) {
        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);
        IR_ExprResult arg_er = {0};

        curr_bb = IR_emit_expr(builder, curr_bb, arg->expr, &arg_er, tmp_obj_list);

        syscall_args[arg_index] = IR_expr_result_to_op_ria(builder, &curr_bb, &arg_er);

        arg_index += 1;
        it = it->next;
    }

    assert(it == args);

    dst_er->type = builtin_types[BUILTIN_TYPE_SSIZE].type;
    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->reg = IR_next_reg(builder);

    IR_emit_instr_syscall(builder, curr_bb, dst_er->reg, syscall_nr, ARRAY_LEN(syscall_args), syscall_args);

    return curr_bb;
}

static BBlock* IR_emit_syscall_call(IR_ProcBuilder* builder, BBlock* bblock, IR_ExprResult* dst_er, size_t num_args, List* args,
                                    IR_TmpObjList* tmp_obj_list)
{
    BBlock* curr_bb = bblock;
    OpRIA nr_and_args[7];

    assert(num_args <= 7);

    List* it = args->next;
    size_t arg_index = 0;

    while (arg_index < num_args) {
        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);
        IR_ExprResult arg_er = {0};

        curr_bb = IR_emit_expr(builder, curr_bb, arg->expr, &arg_er, tmp_obj_list);

        assert(arg_er.type == builtin_types[BUILTIN_TYPE_SSIZE].type);
        nr_and_args[arg_index] = IR_expr_result_to_op_ria(builder, &curr_bb, &arg_er);

        arg_index += 1;
        it = it->next;
    }

    assert(it == args);

    OpRIA syscall_nr = nr_and_args[0];
    u8 num_syscall_args = (u8)(num_args - 1);
    OpRIA* syscall_args = &nr_and_args[1];

    dst_er->type = builtin_types[BUILTIN_TYPE_SSIZE].type;
    dst_er->kind = IR_EXPR_RESULT_REG;
    dst_er->reg = IR_next_reg(builder);

    IR_emit_instr_syscall(builder, curr_bb, dst_er->reg, syscall_nr, num_syscall_args, syscall_args);

    return curr_bb;
}

static IR_Value IR_setup_call_ret(IR_ProcBuilder* builder, Type* ret_type, IR_ExprResult* dst_er, IR_TmpObjList* tmp_obj_list)
{
    IR_Value ret_val = {.type = ret_type};
    dst_er->type = ret_type;

    // Allocate register if procedure returns a value.
    if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
        if (!type_is_obj_like(dst_er->type)) {
            dst_er->kind = IR_EXPR_RESULT_REG;
            dst_er->reg = IR_next_reg(builder);

            ret_val.reg = dst_er->reg;
        }
        else {
            dst_er->kind = IR_EXPR_RESULT_TMP_OBJ;
            dst_er->tmp_obj = IR_get_tmp_obj(builder, tmp_obj_list, ret_type->size, ret_type->align);

            ret_val.addr = IR_tmp_obj_as_addr(dst_er->tmp_obj);
        }
    }

    return ret_val;
}

static IR_Value* IR_setup_call_args(IR_ProcBuilder* builder, BBlock** p_bblock, ExprCall* expr_call, size_t* p_num_args,
                                    IR_TmpObjList* tmp_obj_list)
{
    Type* proc_type = expr_call->proc->type;
    bool is_variadic = proc_type->as_proc.is_variadic;
    size_t num_params = proc_type->as_proc.num_params;
    size_t num_args = expr_call->num_args;

    // NOTE: Variadic arguments are collapsed into one struct, so the number of IR arguments will always
    // match the number of parameters.
    *p_num_args = num_params;
    IR_Value* args = alloc_array(builder->arena, IR_Value, *p_num_args, false);

    // Emit instructions for each argument expression and collect the resulting expression values
    // into an `args` array.
    size_t n = is_variadic ? num_params - 1 : num_params;
    size_t arg_index = 0;
    List* head = &expr_call->args;
    List* it = head->next;

    // Handle arguments for non-variadic parameters.
    while (arg_index < n) {
        assert(it != head && arg_index < num_args);
        ProcCallArg* ast_arg = list_entry(it, ProcCallArg, lnode);
        IR_ExprResult arg_er = {0};

        *p_bblock = IR_emit_expr(builder, *p_bblock, ast_arg->expr, &arg_er, tmp_obj_list);

        if (!type_is_obj_like(arg_er.type)) {
            *p_bblock = IR_expr_result_to_reg(builder, *p_bblock, &arg_er);

            assert(arg_index < num_args);
            args[arg_index].type = arg_er.type;
            args[arg_index].reg = arg_er.reg;
        }
        else {
            args[arg_index].type = arg_er.type;
            IR_get_object_addr(builder, *p_bblock, &args[arg_index].addr, &arg_er);
        }

        arg_index += 1;
        it = it->next;
    }

    // Handle variadic arguments.
    // Place variadic arguments into a slice and pass the slice to the procedure.
    if (is_variadic) {
        //
        // var struct_arg : []<elem_type> = {.length = <num_vargs>, .data = arr};
        //
        size_t num_vargs = num_args - n;

        Type* struct_type = proc_type->as_proc.params[num_params - 1];
        IR_TmpObj* struct_obj = IR_get_tmp_obj(builder, tmp_obj_list, struct_type->size, struct_type->align);
        MemAddr struct_addr = IR_tmp_obj_as_addr(struct_obj);

        TypeAggregateField* data_field = get_type_struct_field(struct_type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA]);
        TypeAggregateField* length_field = get_type_struct_field(struct_type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_LENGTH]);

        IR_ExprResult length_field_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = length_field->type, .addr = struct_addr};
        length_field_er.addr.disp += length_field->offset;

        IR_ExprResult length_val_er = {.kind = IR_EXPR_RESULT_IMM, .type = length_field->type, .imm.as_int._u64 = num_vargs};

        IR_ExprResult data_field_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = data_field->type, .addr = struct_addr};
        data_field_er.addr.disp += data_field->offset;

        IR_ExprResult data_val_er = {.type = data_field->type}; // The following sets the kind and the addr/NULL.

        if (num_vargs) {
            //
            // var arr : [num_vargs] <elem_type> = {[0] = <varg0>, [1] = <varg1>, ..., [num_vargs - 1] = <..>};
            //

            Type* ptr_type = data_field->type;
            Type* elem_type = ptr_type->as_ptr.base;
            Type* type_any = builtin_types[BUILTIN_TYPE_ANY].type;
            bool elem_is_any = elem_type == type_any;

            // Reserve memory space for the array object.
            IR_TmpObj* arr_obj = IR_get_tmp_obj(builder, tmp_obj_list, elem_type->size * num_vargs, elem_type->align);
            MemAddr arr_addr = IR_tmp_obj_as_addr(arr_obj);
            IR_ExprResult elem_ptr_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = elem_type, .addr = arr_addr};

            // Assign each argument expression to the corresponding element in the array.
            while (arg_index < num_args) {
                assert(it != head && arg_index < num_args);
                ProcCallArg* ast_arg = list_entry(it, ProcCallArg, lnode);
                IR_ExprResult arg_er = {0};

                *p_bblock = IR_emit_expr(builder, *p_bblock, ast_arg->expr, &arg_er, tmp_obj_list);

                if (elem_is_any && arg_er.type != elem_type) {
                    //
                    // var cpy_obj := arg;
                    // var any_obj : ^Any = ^arr[arg_index];
                    //
                    // any_obj.type = #typeid(arg_type);
                    // any_obj.ptr = ^cpy_obj;
                    //

                    // We need to load the arg's address into the `any` object's ptr field.
                    // Copy the argument into memory. Copy elision should kick in if the arg is already in a tmp object.
                    IR_TmpObj* cpy_obj = IR_get_tmp_obj(builder, tmp_obj_list, arg_er.type->size, arg_er.type->align);
                    IR_ExprResult cpy_obj_er = {.kind = IR_EXPR_RESULT_TMP_OBJ, .type = arg_er.type, .tmp_obj = cpy_obj};

                    *p_bblock = IR_emit_assign(builder, *p_bblock, &cpy_obj_er, &arg_er); // Do the copy.

                    // Initialize the `any` object's fields. Note that we're directly modifying the array element.
                    MemAddr any_obj_addr = elem_ptr_er.addr;

                    // Set the object's type field to the arg's typeid.
                    TypeAggregateField* type_field = get_type_struct_field(type_any, builtin_struct_fields[BUILTIN_STRUCT_FIELD_TYPE]);
                    IR_ExprResult type_field_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = type_field->type, .addr = any_obj_addr};
                    IR_ExprResult type_val_er = {.kind = IR_EXPR_RESULT_IMM,
                                                 .type = type_field->type,
                                                 .imm.as_int._u64 = arg_er.type->id};

                    type_field_er.addr.disp += type_field->offset;
                    *p_bblock = IR_emit_assign(builder, *p_bblock, &type_field_er, &type_val_er);

                    // Set the object's ptr field to the arg's address.
                    TypeAggregateField* ptr_field = get_type_struct_field(type_any, builtin_struct_fields[BUILTIN_STRUCT_FIELD_PTR]);
                    IR_ExprResult ptr_field_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = ptr_field->type, .addr = any_obj_addr};
                    IR_ExprResult ptr_val_er = {.kind = IR_EXPR_RESULT_MEM_ADDR,
                                                .type = ptr_field->type,
                                                .addr = IR_tmp_obj_as_addr(cpy_obj)};

                    ptr_field_er.addr.disp += ptr_field->offset;
                    *p_bblock = IR_emit_assign(builder, *p_bblock, &ptr_field_er, &ptr_val_er);
                }
                else {
                    *p_bblock = IR_emit_assign(builder, *p_bblock, &elem_ptr_er, &arg_er);
                }

                elem_ptr_er.addr.disp += elem_type->size;

                arg_index += 1;
                it = it->next;
            }

            data_val_er.kind = IR_EXPR_RESULT_MEM_ADDR;
            data_val_er.addr = arr_addr; // struct_arg.data points to the array
        }
        else {
            data_val_er.kind = IR_EXPR_RESULT_IMM;
            data_val_er.imm.as_int._u64 = 0; // struct_arg.data is NULL
        }

        *p_bblock = IR_emit_assign(builder, *p_bblock, &length_field_er, &length_val_er);
        *p_bblock = IR_emit_assign(builder, *p_bblock, &data_field_er, &data_val_er);

        args[n].type = struct_type;
        args[n].addr = struct_addr;
    }

    return args;
}

static BBlock* IR_emit_expr_call(IR_ProcBuilder* builder, BBlock* bblock, ExprCall* expr_call, IR_ExprResult* dst_er,
                                 IR_TmpObjList* tmp_obj_list)
{
    BBlock* curr_bb = bblock;

    // Emit instructions for the procedure pointer/name.
    IR_ExprResult proc_er = {0};
    curr_bb = IR_emit_expr(builder, curr_bb, expr_call->proc, &proc_er, tmp_obj_list);

    if ((proc_er.kind == IR_EXPR_RESULT_PROC) && (proc_er.sym->name == intrinsic_idents[INTRINSIC_MEMCPY])) {
        curr_bb = IR_emit_memcpy_call(builder, curr_bb, expr_call->num_args, &expr_call->args, tmp_obj_list);
    }
    else if ((proc_er.kind == IR_EXPR_RESULT_PROC) && (proc_er.sym->name == intrinsic_idents[INTRINSIC_MEMSET])) {
        curr_bb = IR_emit_memset_call(builder, curr_bb, expr_call->num_args, &expr_call->args, tmp_obj_list);
    }
    else if ((proc_er.kind == IR_EXPR_RESULT_PROC) && (proc_er.sym->name == intrinsic_idents[INTRINSIC_WRITEOUT])) {
        curr_bb = IR_emit_writeout_call(builder, curr_bb, dst_er, expr_call->num_args, &expr_call->args, tmp_obj_list);
    }
    else if ((proc_er.kind == IR_EXPR_RESULT_PROC) && (proc_er.sym->name == intrinsic_idents[INTRINSIC_READIN])) {
        curr_bb = IR_emit_readin_call(builder, curr_bb, dst_er, expr_call->num_args, &expr_call->args, tmp_obj_list);
    }
    else if ((proc_er.kind == IR_EXPR_RESULT_PROC) &&
             (proc_er.sym->name == intrinsic_idents[INTRINSIC_SYSCALL0] || proc_er.sym->name == intrinsic_idents[INTRINSIC_SYSCALL1] ||
              proc_er.sym->name == intrinsic_idents[INTRINSIC_SYSCALL2] || proc_er.sym->name == intrinsic_idents[INTRINSIC_SYSCALL3] ||
              proc_er.sym->name == intrinsic_idents[INTRINSIC_SYSCALL4] || proc_er.sym->name == intrinsic_idents[INTRINSIC_SYSCALL5] ||
              proc_er.sym->name == intrinsic_idents[INTRINSIC_SYSCALL6])) {
        curr_bb = IR_emit_syscall_call(builder, curr_bb, dst_er, expr_call->num_args, &expr_call->args, tmp_obj_list);
    }
    else {
        size_t num_args = 0;
        IR_Value* args = IR_setup_call_args(builder, &curr_bb, expr_call, &num_args, tmp_obj_list);

        // Direct procedure call.
        if (proc_er.kind == IR_EXPR_RESULT_PROC) {
            IR_Value r = IR_setup_call_ret(builder, expr_call->super.type, dst_er, tmp_obj_list);
            IR_emit_instr_call(builder, curr_bb, proc_er.sym, r, num_args, args);
        }
        // Indirect procedure call through register.
        else {
            curr_bb = IR_expr_result_to_reg(builder, curr_bb, &proc_er);
            IR_Value r = IR_setup_call_ret(builder, expr_call->super.type, dst_er, tmp_obj_list);
            IR_emit_instr_call_indirect(builder, curr_bb, proc_er.type, proc_er.reg, r, num_args, args);
        }

        builder->curr_proc->as_proc.is_nonleaf = true;
    }

    return curr_bb;
}

static BBlock* IR_emit_expr_array_lit(IR_ProcBuilder* builder, BBlock* bblock, ExprCompoundLit* expr, IR_ExprResult* dst,
                                      IR_TmpObjList* tmp_obj_list)
{
    assert(expr->super.type->kind == TYPE_ARRAY);
    BBlock* curr_bb = bblock;

    Type* arr_type = expr->super.type;
    Type* elem_type = arr_type->as_array.base;

    // Create a temporary object for the array initializer.
    IR_TmpObj* arr_obj = IR_get_tmp_obj(builder, tmp_obj_list, arr_type->size, arr_type->align);
    MemAddr arr_addr = IR_tmp_obj_as_addr(arr_obj);

    size_t num_initzers = expr->num_initzers;
    size_t num_elems = arr_type->as_array.len;

    // Just memset to 0 if don't have any initializers and the array has more than 4 elements.
    if (num_initzers == 0) {
        IR_zero_memory(builder, curr_bb, &arr_addr, arr_type->size);

        dst->kind = IR_EXPR_RESULT_TMP_OBJ;
        dst->type = arr_type;
        dst->tmp_obj = arr_obj;

        return curr_bb;
    }

    assert(num_initzers <= num_elems);
    size_t num_zero_elems = num_elems - num_initzers;
    bool zero_first_pass = (num_zero_elems > 4);

    // If we have more than 4 uninitialized elems, clear entire array with memset before initializing individual elems.
    if (zero_first_pass) {
        IR_zero_memory(builder, curr_bb, &arr_addr, arr_type->size);

        List* head = &expr->initzers;
        List* it = head->next;
        u64 elem_index = 0;

        while (it != head) {
            MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);

            if (initzer->designator.kind == DESIGNATOR_INDEX) {
                IR_ExprResult desig_er = {0};
                curr_bb = IR_emit_expr(builder, curr_bb, initzer->designator.index, &desig_er, tmp_obj_list);

                assert(desig_er.kind == IR_EXPR_RESULT_IMM);
                elem_index = desig_er.imm.as_int._u64;
            }
            else {
                assert(initzer->designator.kind == DESIGNATOR_NONE);
            }

            // Emit IR for the initializer value
            IR_ExprResult elem_val_er = {0};
            curr_bb = IR_emit_expr(builder, curr_bb, initzer->init, &elem_val_er, tmp_obj_list);

            // Initialize array element with value of the initializer.
            IR_ExprResult elem_ptr_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = elem_type, .addr = arr_addr};
            elem_ptr_er.addr.disp += elem_type->size * elem_index;
            curr_bb = IR_emit_assign(builder, curr_bb, &elem_ptr_er, &elem_val_er);

            elem_index += 1;
            it = it->next;
        }
    }
    else {
        // Create array of bit flags: 1 bit per element in array.
        // Bit will be set to 1 if the array element has an initializer.
        BitArray inited_elems = {0};
        bit_arr_init(&inited_elems, builder->tmp_arena, num_elems);

        // Iterate through initializers and: 1. mark element as having an initializer, 2. initialize element.
        List* head = &expr->initzers;
        List* it = head->next;
        u64 elem_index = 0;

        while (it != head) {
            MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);

            if (initzer->designator.kind == DESIGNATOR_INDEX) {
                IR_ExprResult desig_er = {0};
                curr_bb = IR_emit_expr(builder, curr_bb, initzer->designator.index, &desig_er, tmp_obj_list);

                assert(desig_er.kind == IR_EXPR_RESULT_IMM);
                elem_index = desig_er.imm.as_int._u64;
            }
            else {
                assert(initzer->designator.kind == DESIGNATOR_NONE);
            }

            // Mark array element as having an initializer.
            bit_arr_set(&inited_elems, elem_index, true);

            // Emit IR for the initializer value
            IR_ExprResult elem_val_er = {0};
            curr_bb = IR_emit_expr(builder, curr_bb, initzer->init, &elem_val_er, tmp_obj_list);

            // Initialize array element with value of the initializer.
            IR_ExprResult elem_ptr_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = elem_type, .addr = arr_addr};
            elem_ptr_er.addr.disp += elem_type->size * elem_index;
            curr_bb = IR_emit_assign(builder, curr_bb, &elem_ptr_er, &elem_val_er);

            elem_index += 1;
            it = it->next;
        }

        // For each array element, compute the pointer to the corresponding element and clear it to zero.
        for (elem_index = 0; elem_index < num_elems; elem_index += 1) {
            // Skip array elements that have been initialized.
            if (bit_arr_get(&inited_elems, elem_index)) {
                continue;
            }

            MemAddr elem_addr = arr_addr;
            elem_addr.disp += elem_type->size * elem_index;

            IR_zero_memory(builder, curr_bb, &elem_addr, elem_type->size);
        }
    }

    dst->kind = IR_EXPR_RESULT_TMP_OBJ;
    dst->type = arr_type;
    dst->tmp_obj = arr_obj;

    return curr_bb;
}

static BBlock* IR_emit_expr_struct_lit(IR_ProcBuilder* builder, BBlock* bblock, ExprCompoundLit* expr, IR_ExprResult* dst,
                                       IR_TmpObjList* tmp_obj_list)
{
    assert(expr->super.type->kind == TYPE_STRUCT);

    Type* struct_type = expr->super.type;
    BBlock* curr_bb = bblock;

    // Allocate a temporary object for the struct literal object.
    IR_TmpObj* struct_obj = IR_get_tmp_obj(builder, tmp_obj_list, struct_type->size, struct_type->align);
    MemAddr struct_addr = IR_tmp_obj_as_addr(struct_obj);

    TypeAggregateBody* type_agg = &struct_type->as_struct.body;
    TypeAggregateField* fields = type_agg->fields;

    size_t num_fields = type_agg->num_fields;
    size_t num_initzers = expr->num_initzers;

    // Memset to 0 if did not specify any initializers.
    if (num_initzers == 0) {
        IR_zero_memory(builder, curr_bb, &struct_addr, struct_type->size);

        dst->kind = IR_EXPR_RESULT_TMP_OBJ;
        dst->type = struct_type;
        dst->tmp_obj = struct_obj;

        return curr_bb;
    }

    List* head = &expr->initzers;
    List* it = head->next;
    size_t field_index = 0;

    // Collect initializer values into the array 'field_ops'. The 'field_ops' array has an element
    // for each field in the struct type. Elements without an initializer are left as 'NULL'.
    IR_ExprResult** field_ops = alloc_array(builder->tmp_arena, IR_ExprResult*, num_fields, true);

    while (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        TypeAggregateField* field;

        if (initzer->designator.kind == DESIGNATOR_NAME) {
            field = get_type_struct_field(struct_type, initzer->designator.name);
            assert(field);

            field_index = field->index + 1;
        }
        else {
            assert(initzer->designator.kind == DESIGNATOR_NONE);
            assert(field_index < num_fields);

            field = &fields[field_index++];
        }

        field_ops[field->index] = alloc_type(builder->tmp_arena, IR_ExprResult, true);
        curr_bb = IR_emit_expr(builder, curr_bb, initzer->init, field_ops[field->index], tmp_obj_list);

        it = it->next;
    }

    assert(num_initzers <= num_fields);
    size_t num_zero_fields = num_fields - num_initzers;
    bool zero_first_pass = (num_zero_fields > 4);

    // Clear the struct memory (as a first pass) if more than 4 fields are uninitialized.
    if (zero_first_pass) {
        IR_zero_memory(builder, curr_bb, &struct_addr, struct_type->size);
    }

    // Initialize each field with the provided initializer OR the default 'zero' value.
    for (size_t i = 0; i < num_fields; i++) {
        TypeAggregateField* field = fields + i;
        IR_ExprResult field_ptr_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = field->type, .addr = struct_addr};
        field_ptr_er.addr.disp += field->offset;

        if (field_ops && field_ops[i]) { // field <= initializer.
            curr_bb = IR_emit_assign(builder, curr_bb, &field_ptr_er, field_ops[i]);
        }
        else if (!zero_first_pass) { // field <= 0
            IR_zero_memory(builder, curr_bb, &field_ptr_er.addr, field->type->size);
        }
    }

    dst->kind = IR_EXPR_RESULT_TMP_OBJ;
    dst->type = struct_type;
    dst->tmp_obj = struct_obj;

    return curr_bb;
}

static BBlock* IR_emit_expr_union_lit(IR_ProcBuilder* builder, BBlock* bblock, ExprCompoundLit* expr, IR_ExprResult* dst,
                                      IR_TmpObjList* tmp_obj_list)
{
    Type* union_type = expr->super.type;

    assert(union_type->kind == TYPE_UNION);

    BBlock* curr_bb = bblock;

    // Allocate a temporary object for the union literal object.
    IR_TmpObj* union_obj = IR_get_tmp_obj(builder, tmp_obj_list, union_type->size, union_type->align);
    MemAddr union_addr = IR_tmp_obj_as_addr(union_obj);

    List* head = &expr->initzers;
    List* it = head->next;

    if (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        TypeAggregateField* field = NULL;

        if (initzer->designator.kind == DESIGNATOR_NAME) {
            field = get_type_union_field(union_type, initzer->designator.name);
        }
        else {
            assert(initzer->designator.kind == DESIGNATOR_NONE);
            field = &union_type->as_union.body.fields[0];
        }

        assert(field);

        IR_ExprResult field_val_er = {0};
        curr_bb = IR_emit_expr(builder, curr_bb, initzer->init, &field_val_er, tmp_obj_list);

        IR_ExprResult field_ptr_er = {.kind = IR_EXPR_RESULT_DEREF_ADDR, .type = field->type, .addr = union_addr};
        field_ptr_er.addr.disp +=
            field->offset; // Union fields all have an offset of 0, but keep this just in case one day they don't.

        curr_bb = IR_emit_assign(builder, curr_bb, &field_ptr_er, &field_val_er);
    }
    else {
        IR_zero_memory(builder, curr_bb, &union_addr, union_type->size);
    }

    dst->kind = IR_EXPR_RESULT_TMP_OBJ;
    dst->type = union_type;
    dst->tmp_obj = union_obj;

    return curr_bb;
}

static BBlock* IR_emit_expr_compound_lit(IR_ProcBuilder* builder, BBlock* bblock, ExprCompoundLit* expr, IR_ExprResult* dst,
                                         IR_TmpObjList* tmp_obj_list)
{
    Type* type = expr->super.type;

    if (type->kind == TYPE_ARRAY) {
        return IR_emit_expr_array_lit(builder, bblock, expr, dst, tmp_obj_list);
    }
    else if (type->kind == TYPE_STRUCT) {
        return IR_emit_expr_struct_lit(builder, bblock, expr, dst, tmp_obj_list);
    }
    else {
        assert(type->kind == TYPE_UNION);
        return IR_emit_expr_union_lit(builder, bblock, expr, dst, tmp_obj_list);
    }
}

static BBlock* IR_emit_expr(IR_ProcBuilder* builder, BBlock* bblock, Expr* expr, IR_ExprResult* dst, IR_TmpObjList* tmp_obj_list)
{
    if (expr->is_constexpr && expr->is_imm) {
        Type* type = expr->type;
        Scalar imm = expr->imm;

        if (type->kind == TYPE_FLOAT) {
            FloatLit* float_lit = intern_float_lit(builder->float_lit_map, type->as_float.kind, imm.as_float);

            dst->kind = IR_EXPR_RESULT_FLOAT_LIT;
            dst->type = type;
            dst->float_lit = float_lit;

            if (!float_lit->used) {
                float_lit->used = true;
                float_lit->index = builder->float_lits->list.num_elems;
                add_global_data(builder->float_lits, float_lit, float_kind_sizes[float_lit->kind]);
            }
        }
        else {
            assert(type_is_scalar(type));
            dst->kind = IR_EXPR_RESULT_IMM;
            dst->type = type;
            dst->imm = imm;
        }

        return bblock;
    }

    switch (expr->kind) {
    case CST_ExprIdent:
        IR_emit_expr_ident(builder, (ExprIdent*)expr, dst);
        return bblock;
    case CST_ExprCall:
        return IR_emit_expr_call(builder, bblock, (ExprCall*)expr, dst, tmp_obj_list);
    case CST_ExprCast:
        return IR_emit_expr_cast(builder, bblock, (ExprCast*)expr, dst, tmp_obj_list);
    case CST_ExprBitCast:
        return IR_emit_expr_bit_cast(builder, bblock, (ExprBitCast*)expr, dst, tmp_obj_list);
    case CST_ExprUnary:
        return IR_emit_expr_unary(builder, bblock, (ExprUnary*)expr, dst, tmp_obj_list);
    case CST_ExprBinary:
        return IR_emit_expr_binary(builder, bblock, (ExprBinary*)expr, dst, tmp_obj_list);
    case CST_ExprTernary:
        return IR_emit_expr_ternary(builder, bblock, (ExprTernary*)expr, dst, tmp_obj_list);
    case CST_ExprIndex:
        return IR_emit_expr_index(builder, bblock, (ExprIndex*)expr, dst, tmp_obj_list);
    case CST_ExprField:
        return IR_emit_expr_field(builder, bblock, (ExprField*)expr, dst, tmp_obj_list);
    case CST_ExprFieldIndex:
        return IR_emit_expr_field_index(builder, bblock, (ExprFieldIndex*)expr, dst, tmp_obj_list);
    case CST_ExprCompoundLit:
        return IR_emit_expr_compound_lit(builder, bblock, (ExprCompoundLit*)expr, dst, tmp_obj_list);
    case CST_ExprStr: {
        ExprStr* expr_str_lit = (ExprStr*)expr;
        StrLit* str_lit = expr_str_lit->str_lit;

        dst->kind = IR_EXPR_RESULT_STR_LIT;
        dst->type = expr_str_lit->super.type;
        dst->str_lit = str_lit;

        if (!str_lit->used) {
            str_lit->used = true;
            str_lit->index = builder->str_lits->list.num_elems;
            add_global_data(builder->str_lits, str_lit, str_lit->len + 1);
        }

        return bblock;
    }
    default:
        NIBBLE_FATAL_EXIT("Unsupported expr kind %d during code generation\n", expr->kind);
        return NULL;
    }
}

static BBlock* IR_emit_stmt_block_body(IR_ProcBuilder* builder, BBlock* bblock, List* stmts, IR_UJmpList* break_ujmps,
                                       IR_UJmpList* cont_ujmps)
{
    BBlock* last_bb = bblock;

    for (List* it = stmts->next; it != stmts; it = it->next) {
        Stmt* s = list_entry(it, Stmt, lnode);
        last_bb = IR_emit_stmt(builder, last_bb, s, break_ujmps, cont_ujmps);
    }

    return last_bb;
}

static BBlock* IR_emit_stmt_block(IR_ProcBuilder* builder, BBlock* bblock, StmtBlock* sblock, IR_UJmpList* break_ujmps,
                                  IR_UJmpList* cont_ujmps)
{
    IR_push_scope(builder, sblock->scope);
    BBlock* last_bb = IR_emit_stmt_block_body(builder, bblock, &sblock->stmts, break_ujmps, cont_ujmps);
    IR_pop_scope(builder);

    return last_bb;
}

static BBlock* IR_emit_stmt_return(IR_ProcBuilder* builder, BBlock* bblock, StmtReturn* sret, IR_TmpObjList* tmp_obj_list)
{
    BBlock* last_bb = bblock;
    IR_Value ret_val = {.type = builtin_types[BUILTIN_TYPE_VOID].type};

    if (sret->expr) {
        IR_ExprResult expr_er = {0};

        last_bb = IR_emit_expr(builder, last_bb, sret->expr, &expr_er, tmp_obj_list);
        ret_val.type = expr_er.type;

        if (type_is_obj_like(expr_er.type)) {
            IR_get_object_addr(builder, last_bb, &ret_val.addr, &expr_er);
        }
        else {
            last_bb = IR_expr_result_to_reg(builder, last_bb, &expr_er);
            ret_val.reg = expr_er.reg;
        }
    }

    IR_emit_instr_ret(builder, last_bb, ret_val);

    return NULL;
}

static BBlock* IR_emit_stmt_decl(IR_ProcBuilder* builder, BBlock* bblock, StmtDecl* sdecl, IR_TmpObjList* tmp_obj_list)
{
    if (sdecl->decl->kind == CST_DeclConst) {
        return bblock;
    }

    assert(sdecl->decl->kind == CST_DeclVar);

    DeclVar* dvar = (DeclVar*)sdecl->decl;

    // Early exit if variable is explicitly uninitialized.
    if (dvar->flags & DECL_VAR_IS_UNINIT) {
        return bblock;
    }

    BBlock* last_bb = bblock;

    IR_ExprResult lhs_er = {0};
    IR_expr_result_from_sym(&lhs_er, lookup_symbol(builder->curr_scope, dvar->super.name));

    if (dvar->init) {
        IR_ExprResult rhs_er = {0};

        last_bb = IR_emit_expr(builder, last_bb, dvar->init, &rhs_er, tmp_obj_list);
        last_bb = IR_emit_assign(builder, last_bb, &lhs_er, &rhs_er);
    }
    else {
        MemAddr addr = {0};
        IR_get_object_addr(builder, last_bb, &addr, &lhs_er);
        IR_zero_memory(builder, last_bb, &addr, lhs_er.type->size);
    }

    return last_bb;
}

static BBlock* IR_emit_stmt_expr(IR_ProcBuilder* builder, BBlock* bblock, StmtExpr* sexpr, IR_TmpObjList* tmp_obj_list)
{
    IR_ExprResult expr_er = {0};
    BBlock* curr_bb = IR_emit_expr(builder, bblock, sexpr->expr, &expr_er, tmp_obj_list);

    // Actually execute any deferred operations.
    switch (expr_er.kind) {
    case IR_EXPR_RESULT_DEREF_ADDR:
        IR_execute_deref(builder, curr_bb, &expr_er);
        break;
    case IR_EXPR_RESULT_DEFERRED_CMP:
        curr_bb = IR_execute_deferred_cmp(builder, curr_bb, &expr_er);
        break;
    case IR_EXPR_RESULT_MEM_ADDR:
        IR_execute_lea(builder, curr_bb, &expr_er);
        break;
    default:
        break;
    }

    return curr_bb;
}

static BBlock* IR_emit_stmt_expr_assign(IR_ProcBuilder* builder, BBlock* bblock, StmtExprAssign* stmt, IR_TmpObjList* tmp_obj_list)
{
    // TODO: Create a separate (smaller) enum for compound assignment kinds.
    static IR_EmitBinOpProc* bin_procs[TKN_KIND_COUNT] = {
        [TKN_ADD_ASSIGN] = IR_emit_op_add,   [TKN_SUB_ASSIGN] = IR_emit_op_sub, [TKN_MUL_ASSIGN] = IR_emit_op_mul,
        [TKN_DIV_ASSIGN] = IR_emit_op_div,   [TKN_MOD_ASSIGN] = IR_emit_op_mod, [TKN_AND_ASSIGN] = IR_emit_op_and,
        [TKN_OR_ASSIGN] = IR_emit_op_or,     [TKN_XOR_ASSIGN] = IR_emit_op_xor, [TKN_RSHIFT_ASSIGN] = IR_emit_op_sar,
        [TKN_LSHIFT_ASSIGN] = IR_emit_op_shl};

    BBlock* last_bb = bblock;
    TokenKind op_assign = stmt->op_assign;

    switch (op_assign) {
    case TKN_ASSIGN: {
        IR_ExprResult lhs_er = {0};
        IR_ExprResult rhs_er = {0};

        last_bb = IR_emit_expr(builder, last_bb, stmt->left, &lhs_er, tmp_obj_list);
        last_bb = IR_emit_expr(builder, last_bb, stmt->right, &rhs_er, tmp_obj_list);
        last_bb = IR_emit_assign(builder, last_bb, &lhs_er, &rhs_er);
        break;
    }

    // NOTE: Side-effects of lhs must occur only once.
    //
    // EX: Assume foo() returns a monotonically increasing integer every time it is called.
    //
    // var arr : [3]int = ...;
    // arr[foo()] += 1.0;
    //
    // Should become =>
    //
    // var _ptr : ^int = ^arr[foo()];
    // *_ptr = *_ptr + 1.0
    //
    case TKN_ADD_ASSIGN:
    case TKN_SUB_ASSIGN: {
        IR_ExprResult lhs_er = {0};
        IR_ExprResult rhs_er = {0};

        last_bb = IR_emit_expr(builder, last_bb, stmt->left, &lhs_er, tmp_obj_list);
        last_bb = IR_emit_expr(builder, last_bb, stmt->right, &rhs_er, tmp_obj_list);

        // Cast "copy" of lhs_er to rhs_er's type if both expr_results are arithmetic.
        IR_ExprResult lhs_cpy_er = lhs_er;
        IR_ExprResult casted_lhs_er = {0};

        if (lhs_cpy_er.type != rhs_er.type && type_is_arithmetic(lhs_cpy_er.type) && type_is_arithmetic(rhs_er.type)) {
            last_bb = IR_emit_op_cast(builder, last_bb, tmp_obj_list, &lhs_cpy_er, &casted_lhs_er, rhs_er.type);
        }
        else {
            casted_lhs_er = lhs_cpy_er;
        }

        // Emit binary instruction.
        IR_ExprResult bin_er = {0};
        IR_EmitBinOpProc* bin_op_proc = bin_procs[stmt->op_assign];
        last_bb = bin_op_proc(builder, last_bb, &casted_lhs_er, &rhs_er, &bin_er, rhs_er.type);

        // Cast the binary operation's result to lhs_er's type.
        IR_ExprResult casted_bin_er = {0};

        if (bin_er.type != lhs_er.type) {
            last_bb = IR_emit_op_cast(builder, last_bb, tmp_obj_list, &bin_er, &casted_bin_er, lhs_er.type);
        }
        else {
            casted_bin_er = bin_er;
        }

        // Assign result to lhs_er.
        last_bb = IR_emit_assign(builder, last_bb, &lhs_er, &casted_bin_er);
        break;
    }
    case TKN_MUL_ASSIGN:
    case TKN_DIV_ASSIGN:
    case TKN_MOD_ASSIGN:
    case TKN_AND_ASSIGN:
    case TKN_OR_ASSIGN:
    case TKN_XOR_ASSIGN: {
        IR_ExprResult lhs_er = {0};
        IR_ExprResult rhs_er = {0};

        last_bb = IR_emit_expr(builder, last_bb, stmt->left, &lhs_er, tmp_obj_list);
        last_bb = IR_emit_expr(builder, last_bb, stmt->right, &rhs_er, tmp_obj_list);

        // Cast "copy" of lhs_er to rhs_er's type if both expr_results are arithmetic.
        IR_ExprResult lhs_cpy_er = lhs_er;
        IR_ExprResult casted_lhs_er = {0};

        if (lhs_cpy_er.type != rhs_er.type) {
            last_bb = IR_emit_op_cast(builder, last_bb, tmp_obj_list, &lhs_cpy_er, &casted_lhs_er, rhs_er.type);
        }
        else {
            casted_lhs_er = lhs_cpy_er;
        }

        // Emit binary instruction.
        IR_ExprResult bin_er = {0};
        IR_EmitBinOpProc* bin_op_proc = bin_procs[stmt->op_assign];

        last_bb = bin_op_proc(builder, last_bb, &casted_lhs_er, &rhs_er, &bin_er, rhs_er.type);

        // Cast the binary operation's result to lhs_er's type.
        IR_ExprResult casted_bin_er = {0};

        if (bin_er.type != lhs_er.type) {
            last_bb = IR_emit_op_cast(builder, last_bb, tmp_obj_list, &bin_er, &casted_bin_er, lhs_er.type);
        }
        else {
            casted_bin_er = bin_er;
        }

        // Assign result to lhs_er.
        last_bb = IR_emit_assign(builder, last_bb, &lhs_er, &casted_bin_er);
        break;
    }
    case TKN_RSHIFT_ASSIGN:
    case TKN_LSHIFT_ASSIGN: {
        IR_ExprResult lhs_er = {0};
        IR_ExprResult rhs_er = {0};

        last_bb = IR_emit_expr(builder, last_bb, stmt->left, &lhs_er, tmp_obj_list);
        last_bb = IR_emit_expr(builder, last_bb, stmt->right, &rhs_er, tmp_obj_list);

        // Cast "copy" of lhs_er to rhs_er's type if both expr_results are arithmetic.
        IR_ExprResult lhs_cpy_er = lhs_er;

        // Emit binary instruction.
        IR_ExprResult bin_er = {0};
        IR_EmitBinOpProc* bin_op_proc = bin_procs[stmt->op_assign];

        last_bb = bin_op_proc(builder, last_bb, &lhs_cpy_er, &rhs_er, &bin_er, lhs_cpy_er.type);

        // Assign result to lhs_er.
        last_bb = IR_emit_assign(builder, last_bb, &lhs_er, &bin_er);
        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unsupported compound assignment operator kind `%d` in IR generator.\n", stmt->op_assign);
        break; // Does not execute.
    }

    return last_bb;
}

static BBlock* IR_process_cfg_cond(IR_ProcBuilder* builder, Expr* expr, BBlock* hdr_bb, bool jmp_result, BBlock* jmp_bb,
                                   IR_TmpObjList* tmp_obj_list)
{
    IR_ExprResult cond_er = {0};
    BBlock* curr_bb = IR_emit_expr(builder, hdr_bb, expr, &cond_er, tmp_obj_list);

    if (cond_er.kind == IR_EXPR_RESULT_DEFERRED_CMP) {
        IR_DeferredJmpcc* final_jmp = &cond_er.cmp.final_jmp;

        IR_fix_sc_jmp_path(final_jmp, jmp_result);

        // Patch final jump target or create it if it doesn't exist.
        if (final_jmp->jmp) {
            IR_patch_jmp_target(final_jmp->jmp, jmp_bb);
        }
        else {
            BBlock* last_bb = IR_alloc_bblock(builder);
            IR_Instr* cmp_instr = final_jmp->cmp;
            IR_Reg cmp_result =
                cmp_instr->kind == IR_InstrIntCmp_KIND ? ((IR_InstrIntCmp*)cmp_instr)->r : ((IR_InstrFltCmp*)cmp_instr)->r;

            final_jmp->jmp = IR_emit_instr_cond_jmp(builder, curr_bb, jmp_bb, last_bb, cmp_result);

            curr_bb = last_bb;
        }

        // Patch short-circuit jumps.
        for (IR_DeferredJmpcc* it = cond_er.cmp.first_sc_jmp; it; it = it->next) {
            IR_patch_jmp_target(it->jmp, (it->result == jmp_result ? jmp_bb : curr_bb));
        }
    }
    else {
        ConditionKind cond_kind = jmp_result ? COND_NEQ : COND_EQ;
        IR_Instr* cmp_instr = IR_cmp_to_zero(builder, &curr_bb, &cond_er, cond_kind);
        IR_Reg cmp_reg = cmp_instr->kind == IR_InstrIntCmp_KIND ? ((IR_InstrIntCmp*)cmp_instr)->r : ((IR_InstrFltCmp*)cmp_instr)->r;

        BBlock* last_bb = IR_alloc_bblock(builder);
        IR_emit_instr_cond_jmp(builder, curr_bb, jmp_bb, last_bb, cmp_reg);

        curr_bb = last_bb;
    }

    return curr_bb;
}

static BBlock* IR_emit_stmt_if(IR_ProcBuilder* builder, BBlock* bblock, StmtIf* stmt, IR_UJmpList* break_ujmps,
                               IR_UJmpList* cont_ujmps, IR_TmpObjList* tmp_obj_list)
{
    Expr* cond_expr = stmt->if_blk.cond;
    Stmt* if_body = stmt->if_blk.body;
    Stmt* else_body = stmt->else_blk.body;

    // If expr is a compile-time constant, do not generate the unneeded branch!!
    if (cond_expr->is_constexpr && cond_expr->is_imm) {
        assert(type_is_scalar(cond_expr->type));
        Stmt* body = cond_expr->imm.as_int._u64 != 0 ? if_body : else_body;

        return IR_emit_stmt(builder, bblock, body, break_ujmps, cont_ujmps);
    }

    BBlock* false_bb = NULL;
    BBlock* last_bb = IR_alloc_bblock(builder);
    BBlock* false_tgt = last_bb;

    if (else_body) {
        false_tgt = false_bb = IR_alloc_bblock(builder);
    }

    // Process condition
    BBlock* true_bb = IR_process_cfg_cond(builder, cond_expr, bblock, false, false_tgt, tmp_obj_list);

    // Emit instructions for if-block body.
    BBlock* true_end_bb = IR_emit_stmt(builder, true_bb, if_body, break_ujmps, cont_ujmps);

    if (true_end_bb) {
        // Generates unnecessary jump without an else-stmt (fall-through) or if if-stmt returns.
        IR_emit_instr_jmp(builder, true_end_bb, last_bb);
    }

    if (else_body) {
        BBlock* false_end_bb = IR_emit_stmt(builder, false_bb, else_body, break_ujmps, cont_ujmps);

        if (false_end_bb) {
            IR_emit_instr_jmp(builder, false_end_bb, last_bb); // Not really needed in actual assembly (fall-through)
        }
        else if (!true_end_bb) {
            // Both paths jump out using break/continue/return.
            // If scope has other statements after if/else, this should be a compiler error in the resolver.
            return NULL;
        }
    }

    return last_bb;
}

static BBlock* IR_emit_stmt_switch(IR_ProcBuilder* builder, BBlock* bblock, StmtSwitch* stmt, IR_UJmpList* break_ujmps,
                                   IR_UJmpList* cont_ujmps, IR_TmpObjList* tmp_obj_list)
{
    Expr* cond_expr = stmt->expr;

    // If expr is a compile-time constant, do not generate the unneeded switch!!
    if (cond_expr->is_constexpr && cond_expr->is_imm) {
        assert(type_is_scalar(cond_expr->type));
        s64 val = cond_expr->imm.as_int._s64;
        SwitchCase* target_case = NULL;

        // Find the target case (binary search)
        s64 lo = 0;
        s64 hi = stmt->num_case_infos - 1;
        while (lo <= hi) {
            s64 mid = lo + ((hi - lo) / 2);
            CaseInfo* case_info = &stmt->case_infos[mid];

            if (val >= case_info->start && val <= case_info->end) {
                target_case = stmt->cases[case_info->index];
                break; // Found our case, eject.
            }

            if (val < case_info->start) { // Look at left half
                hi = mid - 1;
            }
            else { // Look at right half
                assert(val > case_info->end);
                lo = mid + 1;
            }
        }

        // Use default case if necessary
        if (!target_case && stmt->has_default_case) {
            target_case = stmt->cases[stmt->default_case_index];
        }

        if (!target_case) {
            // Not jumping to any case AND we don't have a default case.
            // Therefore, this switch case is a no-op. Maybe we should handle this in the resolver?
            return bblock;
        }

        // Emit all statements for our target case.
        IR_push_scope(builder, target_case->scope);
        BBlock* last_bb = IR_emit_stmt_block_body(builder, bblock, &target_case->stmts, break_ujmps, cont_ujmps);
        IR_pop_scope(builder);

        return last_bb;
    }

#if 1
    NIBBLE_UNUSED_VAR(tmp_obj_list);
    NIBBLE_FATAL_EXIT("Compiler does not yet support general switch statements (coming soon).");
    return NULL;
#else
    BBlock* last_bb = IR_alloc_bblock(builder);
    BBlock* default_bb = NULL;
    BBlock* default_or_last = last_bb;

    if (stmt->has_default_case) {
        default_or_last = default_bb = IR_alloc_bblock(builder);
    }

    // Get expression value
    IR_ExprResult val_er = {0};

    BBlock* bb0 = IR_emit_expr(builder, bblock, stmt->expr, &val_er, tmp_obj_list);
    OpRIA val_ria = IR_expr_result_to_op_ria(builder, &bb0, &val_er);

    // Add offset to make all case values >= 0
    Scalar min_val = {.as_int._s64 = stmt->flat_cases[0].value};
    Scalar max_val = {.as_int._s64 = stmt->flat_cases[stmt->num_flat_cases - 1].value};

    if (min_val.as_int._s64 != 0) {
        IR_Reg dst_reg = IR_next_reg(builder);
        IR_emit_instr_int_sub(builder, bb0, builtin_types[BUILTIN_TYPE_S64].type, dst_reg, val_ria, op_ria_from_imm(min_val));

        val_ria = op_ria_from_reg(dst_reg);
        max_val.as_int._s64 -= min_val.as_int._s64;
    }

    // Jump to default or last bb if val > max_val
    IR_Reg cmp_result = IR_next_reg(builder);
    IR_emit_instr_int_cmp(builder, bb0, builtin_types[BUILTIN_TYPE_S64].type, COND_U_GT, cmp_result, val_ria,
                          op_ria_from_imm(max_val));

    BBlock* bb1 = IR_alloc_bblock(builder);
    IR_emit_instr_cond_jmp(builder, bb0, default_or_last, bb1, cmp_result);

    // TODO: Create new jmp_switch_case IR instruction.
    return last_bb;
#endif
}

static BBlock* IR_emit_inf_loop(IR_ProcBuilder* builder, BBlock* bblock, Stmt* body, Stmt* next)
{
    BBlock* hdr_bblock = IR_alloc_bblock(builder);
    BBlock* end_bblock = IR_alloc_bblock(builder);
    BBlock* nxt_bblock = next ? IR_alloc_bblock(builder) : NULL;

    IR_emit_instr_jmp(builder, bblock, hdr_bblock);

    IR_UJmpList break_ujmps = {0};
    IR_UJmpList cont_ujmps = {0};
    BBlock* body_end_bblock = IR_emit_stmt(builder, hdr_bblock, body, &break_ujmps, &cont_ujmps);

    if (next) {
        if (body_end_bblock) {
            IR_emit_instr_jmp(builder, body_end_bblock, nxt_bblock);
        }

        body_end_bblock = IR_emit_stmt(builder, nxt_bblock, next, NULL, NULL);
        assert(body_end_bblock);
    }

    if (body_end_bblock) {
        IR_emit_instr_jmp(builder, body_end_bblock, hdr_bblock);
        hdr_bblock->flags |= BBLOCK_IS_LOOP_HDR;
    }

    IR_patch_ujmp_list(builder, &break_ujmps, end_bblock);
    IR_patch_ujmp_list(builder, &cont_ujmps, nxt_bblock ? nxt_bblock : hdr_bblock);

    return end_bblock;
}

static BBlock* IR_emit_cond_loop(IR_ProcBuilder* builder, BBlock* bblock, Expr* cond_expr, Stmt* body_stmt, Stmt* next_stmt,
                                 IR_TmpObjList* tmp_obj_list)
{
    BBlock* last_bb = NULL;

    // Emit infinite loop.
    if (!cond_expr || (cond_expr->is_constexpr && cond_expr->is_imm)) {
        assert(!cond_expr || type_is_scalar(cond_expr->type));
        bool cond_val = !cond_expr || (cond_expr->imm.as_int._u64 != 0);

        last_bb = cond_val ? IR_emit_inf_loop(builder, bblock, body_stmt, next_stmt) : bblock;
    }
    // Normal for-loop.
    else {
        // <bblock> -> cond_bb
        // <body_bb>
        // <nxt_bb>
        // <cond_bb> if true -> body_bb, else -> last_bb
        // <last_bb>
        BBlock* body_bb = IR_alloc_bblock(builder);
        BBlock* cond_bb = IR_alloc_bblock(builder);
        BBlock* nxt_bb = next_stmt ? IR_alloc_bblock(builder) : NULL;

        // Jump to the loop condition-check block.
        IR_emit_instr_jmp(builder, bblock, cond_bb);

        // Emit instructions for the loop body.
        //   - break target: last_bb
        //   - continue target: nxt_bb or cond_bb
        IR_UJmpList break_ujmps = {0};
        IR_UJmpList cont_ujmps = {0};
        BBlock* body_end_bb = IR_emit_stmt(builder, body_bb, body_stmt, &break_ujmps, &cont_ujmps);

        // Emit code for the for-loop's 'next' statement.
        if (next_stmt) {
            if (body_end_bb) {
                IR_emit_instr_jmp(builder, body_end_bb, nxt_bb);
            }

            body_end_bb = IR_emit_stmt(builder, nxt_bb, next_stmt, NULL, NULL);
            assert(body_end_bb);
        }

        if (body_end_bb) {
            // Jump to the loop condition-check block.
            IR_emit_instr_jmp(builder, body_end_bb, cond_bb);

            // Explicitly mark loop header.
            cond_bb->flags |= BBLOCK_IS_LOOP_HDR;
        }

        // Process condition
        last_bb = IR_process_cfg_cond(builder, cond_expr, cond_bb, true, body_bb, tmp_obj_list);

        IR_patch_ujmp_list(builder, &break_ujmps, last_bb);
        IR_patch_ujmp_list(builder, &cont_ujmps, nxt_bb ? nxt_bb : cond_bb);
    }

    return last_bb;
}

static BBlock* IR_emit_stmt_for(IR_ProcBuilder* builder, BBlock* bblock, StmtFor* stmt, IR_TmpObjList* tmp_obj_list)
{
    IR_push_scope(builder, stmt->scope);

    if (stmt->init) {
        bblock = IR_emit_stmt(builder, bblock, stmt->init, NULL, NULL);
    }

    BBlock* last_bb = IR_emit_cond_loop(builder, bblock, stmt->cond, stmt->body, stmt->next, tmp_obj_list);

    IR_pop_scope(builder);

    return last_bb;
}

static BBlock* IR_emit_stmt_while(IR_ProcBuilder* builder, BBlock* bblock, StmtWhile* stmt, IR_TmpObjList* tmp_obj_list)
{
    return IR_emit_cond_loop(builder, bblock, stmt->cond, stmt->body, NULL, tmp_obj_list);
}

static BBlock* IR_emit_stmt_do_while(IR_ProcBuilder* builder, BBlock* bblock, StmtDoWhile* stmt, IR_TmpObjList* tmp_obj_list)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body_stmt = stmt->body;

    if (cond_expr->is_constexpr && cond_expr->is_imm) {
        assert(type_is_scalar(cond_expr->type));
        bool cond_val = cond_expr->imm.as_int._u64 != 0;

        // Emit infinite loop
        return cond_val ? IR_emit_inf_loop(builder, bblock, body_stmt, NULL) : bblock;
    }

    BBlock* last_bb;
    BBlock* body_bb = IR_alloc_bblock(builder);

    // Jump to the body basic block.
    IR_emit_instr_jmp(builder, bblock, body_bb);

    // Emit instructions for the loop body.
    //   - break target: last_bb
    //   - continue target: body_bb
    IR_UJmpList break_ujmps = {0};
    IR_UJmpList cont_ujmps = {0};
    BBlock* body_end_bb = IR_emit_stmt(builder, body_bb, body_stmt, &break_ujmps, &cont_ujmps);

    IR_patch_ujmp_list(builder, &cont_ujmps, body_bb);

    if (body_end_bb) {
        body_bb->flags |= BBLOCK_IS_LOOP_HDR;

        // Process condition.
        last_bb = IR_process_cfg_cond(builder, cond_expr, body_end_bb, true, body_bb, tmp_obj_list);
    }
    else {
        last_bb = IR_alloc_bblock(builder);
    }

    IR_patch_ujmp_list(builder, &break_ujmps, last_bb);

    return last_bb;
}

static bool IR_rm_dead_bblocks(Symbol* sym)
{
    bool removed_bblock = false;

    for (size_t i = array_len(sym->as_proc.bblocks); i-- > 0;) {
        BBlock* bb = sym->as_proc.bblocks[i];

        if (bb->num_instrs == 0) {
            array_remove_swap(sym->as_proc.bblocks, i);
            continue;
        }

        if (bb->num_instrs > 1) {
            continue;
        }

        assert(bb->num_instrs == 1);

        IR_Instr* instr = bb->first;

        // This basic block only has a single jump instruction, so we can
        // remove it and make its predecessors jump to the intended target.
        if (instr->kind == IR_InstrJmp_KIND) {
            IR_InstrJmp* jmp = (IR_InstrJmp*)instr;
            BBlock* target = jmp->target;

            BBlock** preds = bb->preds;
            size_t npreds = array_len(preds);

            // Cannot remove if does not have predecessors (e.g., the starting basic block).
            if (!npreds) {
                continue;
            }

            // First, check to make sure we're not removing a basic block that is needed by a fall-through conditional jump.
            bool can_remove = true;

            for (size_t p = 0; p < npreds; p++) {
                BBlock* p_bb = preds[p];
                IR_Instr* p_instr = p_bb->last;

                assert(p_bb->last && p_bb->num_instrs);

                if (p_instr->kind == IR_InstrCondJmp_KIND && ((IR_InstrCondJmp*)p_instr)->false_bb == bb) {
                    can_remove = false;
                    break;
                }
            }

            if (!can_remove) {
                continue;
            }

            // Now, update jmp targets for predecessors.
            for (size_t p = 0; p < npreds; p++) {
                BBlock* p_bb = preds[p];
                IR_Instr* p_instr = p_bb->last;

                // Replace bb's predecessors' jmp targets with `target` instead of `bb`.
                if (p_instr->kind == IR_InstrJmp_KIND) {
                    IR_InstrJmp* p_jmp = (IR_InstrJmp*)p_instr;
                    assert(p_jmp->target == bb); // Should be jumping to bb.
                    p_jmp->target = target; // Skip bb and jump directly to the intended target.
                }
                else {
                    assert(p_instr->kind == IR_InstrCondJmp_KIND);

                    IR_InstrCondJmp* p_jmp = (IR_InstrCondJmp*)p_instr;
                    assert(p_jmp->true_bb == bb);

                    p_jmp->true_bb = target;
                }

                // Add p_bb to target->preds
                array_push(target->preds, p_bb);
            }

            // Remove bb from target->preds (swap with last).
            size_t bb_i = 0;
            size_t n_tgt_preds = array_len(target->preds);

            for (size_t t = 0; t < n_tgt_preds; t++) {
                BBlock* t_bb = target->preds[t];

                if (t_bb == bb) {
                    bb_i = t;
                    break;
                }
            }

            array_remove_swap(target->preds, bb_i);

            // Remove bb from array by swapping with last elem and decrementing count
            array_remove_swap(sym->as_proc.bblocks, i);
            removed_bblock = true;
        }
    }

    return removed_bblock;
}

static BBlock* IR_emit_stmt(IR_ProcBuilder* builder, BBlock* bblock, Stmt* stmt, IR_UJmpList* break_ujmps, IR_UJmpList* cont_ujmps)
{
    BBlock* last_bb = NULL;
    IR_TmpObjList tmp_obj_list = {0};

    switch (stmt->kind) {
    case CST_StmtBlock:
        last_bb = IR_emit_stmt_block(builder, bblock, (StmtBlock*)stmt, break_ujmps, cont_ujmps);
        break;
    case CST_StmtReturn:
        last_bb = IR_emit_stmt_return(builder, bblock, (StmtReturn*)stmt, &tmp_obj_list);
        break;
    case CST_StmtDecl:
        last_bb = IR_emit_stmt_decl(builder, bblock, (StmtDecl*)stmt, &tmp_obj_list);
        break;
    case CST_StmtExpr:
        last_bb = IR_emit_stmt_expr(builder, bblock, (StmtExpr*)stmt, &tmp_obj_list);
        break;
    case CST_StmtExprAssign:
        last_bb = IR_emit_stmt_expr_assign(builder, bblock, (StmtExprAssign*)stmt, &tmp_obj_list);
        break;
    case CST_StmtIf:
        last_bb = IR_emit_stmt_if(builder, bblock, (StmtIf*)stmt, break_ujmps, cont_ujmps, &tmp_obj_list);
        break;
    case CST_StmtSwitch:
        last_bb = IR_emit_stmt_switch(builder, bblock, (StmtSwitch*)stmt, break_ujmps, cont_ujmps, &tmp_obj_list);
        break;
    case CST_StmtWhile:
        last_bb = IR_emit_stmt_while(builder, bblock, (StmtWhile*)stmt, &tmp_obj_list);
        break;
    case CST_StmtDoWhile:
        last_bb = IR_emit_stmt_do_while(builder, bblock, (StmtDoWhile*)stmt, &tmp_obj_list);
        break;
    case CST_StmtFor:
        last_bb = IR_emit_stmt_for(builder, bblock, (StmtFor*)stmt, &tmp_obj_list);
        break;
    case CST_StmtBreak: {
        IR_Instr* instr = IR_emit_instr_jmp(builder, bblock, NULL);
        IR_add_ujmp(builder, break_ujmps, instr); // Add to list of unpatched jumps

        last_bb = NULL;
        break;
    }
    case CST_StmtContinue: {
        IR_Instr* instr = IR_emit_instr_jmp(builder, bblock, NULL);
        IR_add_ujmp(builder, cont_ujmps, instr); // Add to list of unpatched jumps

        last_bb = NULL;
        break;
    }
    case CST_StmtStaticAssert:
        // Do nothing.
        last_bb = bblock;
        break;
    default:
        NIBBLE_FATAL_EXIT("Cannot emit bytecode instruction for statement kind `%d`\n", stmt->kind);
        break;
    }

    // Reset anon object pointer after every statement.
    // This allows us to reuse the same temporary memory for anonymous objects that appear in different statements.
    IR_process_deferred_tmp_objs(builder, &tmp_obj_list);
    IR_reset_proc_tmp_obj_iterator(builder);

    return last_bb;
}

static void IR_build_proc(IR_ProcBuilder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    if (dproc->is_incomplete) {
        return;
    }

    AllocatorState mem_state = allocator_get_state(builder->tmp_arena);

    // Initialize stack of temporary anonymous objects.
    list_head_init(&sym->as_proc.tmp_objs);
    sym->as_proc.num_tmp_objs = 0;

    // Set procedure as the current scope.
    builder->curr_proc = sym;
    IR_push_scope(builder, dproc->scope);

    // Reset iterator that points to the first available tmp obj.
    IR_reset_proc_tmp_obj_iterator(builder);

    // Initialize freelists
    builder->tmp_obj_freelist = NULL;
    builder->sc_jmp_freelist = NULL;
    builder->ujmp_freelist = NULL;

    // Create stretchy buffer to hold basic blocks.
    sym->as_proc.bblocks = array_create(builder->arena, BBlock*, 8);

    // Add the starting basic block.
    BBlock* start_bb = IR_alloc_bblock(builder);
    start_bb->flags |= BBLOCK_IS_START;

    // Emit IR for procedure body.
    BBlock* last_bb = IR_emit_stmt_block_body(builder, start_bb, &dproc->stmts, NULL, NULL);

    // If proc doesn't have explicit returns, add one at the end.
    // NOTE: This should only apply to procs that return void. The resolver
    // will catch other cases.
    if (!dproc->returns) {
        assert(sym->type->as_proc.ret == builtin_types[BUILTIN_TYPE_VOID].type);
        assert(last_bb);
        IR_Value ret_val = {.type = builtin_types[BUILTIN_TYPE_VOID].type};

        IR_emit_instr_ret(builder, last_bb, ret_val);
    }

    IR_pop_scope(builder);
    builder->curr_proc = NULL;

    IR_rm_dead_bblocks(sym);

    // Sort proc bblocks by starting instruction number.
    {
        BBlock** bblocks = sym->as_proc.bblocks;
        size_t n = array_len(bblocks);

        for (size_t i = 0; i < n; i++) {
            for (size_t j = 0; j < n - 1; j++) {
                BBlock* curr = bblocks[j];
                BBlock* next = bblocks[j + 1];

                if (curr->first->ino > next->first->ino) {
                    bblocks[j] = next;
                    bblocks[j + 1] = curr;
                }
            }
        }
    }

    // Renumber bblock IDs and instructions.
    {
        BBlock** bblocks = sym->as_proc.bblocks;
        size_t n = array_len(bblocks);
        long ino = 0;

        for (size_t i = 0; i < n; i++) {
            BBlock* bb = bblocks[i];

            bb->id = i;

            for (IR_Instr* it = bb->first; it; it = it->next) {
                it->ino = ino;
                ino += 2;
            }
        }
    }

#ifdef NIBBLE_PRINT_IRS
    IR_print_out_proc(builder->tmp_arena, sym);
    IR_dump_proc_dot(builder->tmp_arena, sym);
#endif
    allocator_restore_state(mem_state);
}

void IR_build_procs(Allocator* arena, Allocator* tmp_arena, BucketList* procs, GlobalData* str_lits, GlobalData* float_lits,
                    TypeCache* type_cache, HMap* float_lit_map)
{
    IR_ProcBuilder builder = {.arena = arena,
                              .tmp_arena = tmp_arena,
                              .str_lits = str_lits,
                              .float_lits = float_lits,
                              .type_cache = type_cache,
                              .float_lit_map = float_lit_map,
                              .curr_proc = NULL,
                              .curr_scope = NULL};

    // Iterate through all procedures and generate IR instructions.
    for (Bucket* bucket = procs->first; bucket; bucket = bucket->next) {
        for (size_t i = 0; i < bucket->count; i++) {
            Symbol* sym = (Symbol*)(bucket->elems[i]);
            assert(sym->kind == SYMBOL_PROC);

            IR_build_proc(&builder, sym);
        }
    }
}
