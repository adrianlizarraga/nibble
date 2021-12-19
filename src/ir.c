#include "ir.h"

#define NIR_REG_COUNT 0xFFFFFFFF

typedef struct NIR_ProcBuilder {
    Allocator* arena;
    Allocator* tmp_arena;
    TypeCache* type_cache;
    Symbol* curr_proc;
    Scope* curr_scope;

    bool next_instr_is_jmp_target;
    struct NIR_DeferredJmpcc* sc_jmp_freelist;
} NIR_ProcBuilder;

typedef enum NIR_OperandKind {
    NIR_OPERAND_NONE,
    NIR_OPERAND_IMM,
    NIR_OPERAND_REG,
    NIR_OPERAND_MEM_ADDR,
    NIR_OPERAND_DEREF_ADDR,
    NIR_OPERAND_DEFERRED_CMP,
    NIR_OPERAND_ARRAY_INIT,
    NIR_OPERAND_VAR,
    NIR_OPERAND_STR_LIT,
    NIR_OPERAND_PROC,
} NIR_OperandKind;

typedef struct NIR_DeferredJmpcc {
    bool result;
    Instr* jmp; // target needs to be patched. Cond reg needs to be set.
    Instr* cmp; // Condition may need to be inverted.
    struct NIR_DeferredJmpcc* next;
} NIR_DeferredJmpcc;

typedef struct NIR_DeferredCmp {
    NIR_DeferredJmpcc* first_sc_jmp;
    NIR_DeferredJmpcc* last_sc_jmp;
    NIR_DeferredJmpcc final_jmp;
} NIR_DeferredCmp;

typedef struct NIR_ArrayMemberInitializer NIR_ArrayMemberInitializer;

typedef struct NIR_ArrayInitializer {
    u64 num_initzers;
    NIR_ArrayMemberInitializer* initzers;
} NIR_ArrayInitializer;

typedef struct NIR_Operand {
    NIR_OperandKind kind;
    Type* type;

    union {
        Scalar imm;
        NIR_Reg reg;
        MemAddr addr;
        Symbol* sym;
        NIR_DeferredCmp cmp;
        NIR_ArrayInitializer array_initzer;
        StrLit* str_lit;
    };
} NIR_Operand;

typedef struct NIR_ArrayMemberInitializer {
    u64 index;
    NIR_Operand op;
} NIR_ArrayMemberInitializer;

static const Scalar nir_zero_imm = {.as_int._u64 = 0};
static const Scalar nir_one_imm = {.as_int._u64 = 1};

static const ConditionKind nir_opposite_cond[] = {
    [COND_U_LT] = COND_U_GTEQ, [COND_S_LT] = COND_S_GTEQ, [COND_U_LTEQ] = COND_U_GT, [COND_S_LTEQ] = COND_S_GT,
    [COND_U_GT] = COND_U_LTEQ, [COND_S_GT] = COND_S_LTEQ, [COND_U_GTEQ] = COND_U_LT, [COND_S_GTEQ] = COND_S_LT,
    [COND_EQ] = COND_NEQ,      [COND_NEQ] = COND_EQ,
};

//////////////////////////////////////////////////////
//
//         Create IR instructions
//
//////////////////////////////////////////////////////
static void NIR_add_instr(NIR_ProcBuilder* builder, Instr* instr)
{
    if (builder->next_instr_is_jmp_target) {
        instr->is_jmp_target = true;
        builder->next_instr_is_jmp_target = false;
    }

    array_push(builder->curr_proc->as_proc.nir_instrs, instr);
}

static Instr* NIR_new_instr(Allocator* arena, InstrKind kind)
{
    Instr* instr = alloc_type(arena, Instr, true);
    instr->kind = kind;

    return instr;
}

#define NIR_emit_instr_add(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_ADD, (t), (r), (a), (b))
#define NIR_emit_instr_sub(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_SUB, (t), (r), (a), (b))
#define NIR_emit_instr_mul(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_MUL, (t), (r), (a), (b))
#define NIR_emit_instr_udiv(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_UDIV, (t), (r), (a), (b))
#define NIR_emit_instr_sdiv(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_SDIV, (t), (r), (a), (b))
#define NIR_emit_instr_sar(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_SAR, (t), (r), (a), (b))
#define NIR_emit_instr_shl(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_SHL, (t), (r), (a), (b))
#define NIR_emit_instr_and(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_AND, (t), (r), (a), (b))
#define NIR_emit_instr_or(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_OR, (t), (r), (a), (b))
#define NIR_emit_instr_xor(b, t, r, a, b) NIR_emit_instr_binary((b), INSTR_XOR, (t), (r), (a), (b))

static void NIR_emit_instr_binary(NIR_ProcBuilder* builder, InstrKind kind, Type* type, NIR_Reg r, NIR_Reg a, NIR_Reg b)
{
    Instr* instr = NIR_new_instr(builder->arena, kind);
    instr->binary.type = type;
    instr->binary.r = r;
    instr->binary.a = a;
    instr->binary.b = b;

    NIR_add_instr(builder, instr);
}

#define NIR_emit_instr_not(b, t, r, a) NIR_emit_instr_unary((b), INSTR_NOT, (t), (r), (a))
#define NIR_emit_instr_neg(b, t, r, a) NIR_emit_instr_unary((b), INSTR_NEG, (t), (r), (a))

static void NIR_emit_instr_unary(NIR_ProcBuilder* builder, InstrKind kind, Type* type, NIR_Reg r, NIR_Reg a)
{
    Instr* instr = NIR_new_instr(builder->arena, kind);
    instr->unary.type = type;
    instr->unary.r = r;
    instr->unary.a = a;

    NIR_add_instr(builder, instr);
}

#define NIR_emit_instr_trunc(b, dt, st, r, a) NIR_emit_instr_convert((b), INSTR_TRUNC, (dt), (st), (r), (a)) 
#define NIR_emit_instr_zext(b, dt, st, r, a) NIR_emit_instr_convert((b), INSTR_ZEXT, (dt), (st), (r), (a)) 
#define NIR_emit_instr_sext(b, dt, st, r, a) NIR_emit_instr_convert((b), INSTR_SEXT, (dt), (st), (r), (a)) 

static void NIR_emit_instr_convert(NIR_ProcBuilder* builder, InstrKind kind, Type* dst_type, Type* src_type, NIR_Reg r, NIR_Reg a)
{
    Instr* instr = NIR_new_instr(builder->arena, kind);
    instr->convert.dst_type = dst_type;
    instr->convert.src_type = src_type;
    instr->convert.r = r;
    instr->convert.a = a;

    NIR_add_instr(builder, instr);
}

static void NIR_emit_instr_limm(NIR_ProcBuilder* builder, u8 size, NIR_Reg r, Scalar imm)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_LIMM);
    instr->limm.size = size;
    instr->limm.r = r;
    instr->limm.imm = imm;

    NIR_add_instr(builder, instr);
}

static void NIR_emit_instr_load(NIR_ProcBuilder* builder, Type* type, NIR_Reg r, MemAddr addr)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_LOAD);
    instr->load.type = type;
    instr->load.r = r;
    instr->load.addr = addr;
    
    NIR_add_instr(builder, instr);
}

static void NIR_emit_instr_laddr(NIR_ProcBuilder* builder, Type* type, NIR_Reg r, MemAddr addr)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_LADDR);
    instr->laddr.type = type;
    instr->laddr.r = r;
    instr->laddr.addr = addr;
    
    NIR_add_instr(builder, instr);
}

static void NIR_emit_instr_store(NIR_ProcBuilder* builder, Type* type, MemAddr addr, NIR_Reg a)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_STORE);
    instr->store.type = type;
    instr->store.addr = addr;
    instr->store.a = a;

    NIR_add_instr(builder, instr);
}

static Instr* NIR_emit_instr_cmp(NIR_ProcBuilder* builder, Type* type, ConditionKind cond, NIR_Reg r, NIR_Reg a, NIR_Reg b)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_CMP);
    instr->cmp.type = type;
    instr->cmp.cond = cond;
    instr->cmp.r = r;
    instr->cmp.a = a;
    instr->cmp.b = b;

    NIR_add_instr(builder, instr);

    return instr;
}

static Instr* NIR_emit_instr_cond_jmp(NIR_ProcBuilder* builder, u32* jmp_target, NIR_Reg a)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_COND_JMP);
    instr->cond_jmp.jmp_target = jmp_target;
    instr->cond_jmp.a = a;

    NIR_add_instr(builder, instr);

    return instr;
}

static Instr* NIR_emit_instr_jmp(NIR_ProcBuilder* builder, u32* jmp_target)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_JMP);
    instr->jmp.jmp_target = jmp_target;

    NIR_add_instr(builder, instr);

    return instr;
}

static void NIR_emit_instr_call(NIR_ProcBuilder* builder, Symbol* sym, NIR_Reg r, u32 num_args, InstrCallArg* args)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_CALL);
    instr->call.sym = sym;
    instr->call.r = r;
    instr->call.num_args = num_args;
    instr->call.args = args;

    NIR_add_instr(builder, instr);
}

static void NIR_emit_instr_call_indirect(NIR_ProcBuilder* builder, Type* type, NIR_Reg loc, NIR_Reg r, u32 num_args, InstrCallArg* args)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_CALL_INDIRECT);
    instr->calli.proc_type = type;
    instr->calli.loc = loc;
    instr->calli.r = r;
    instr->calli.num_args = num_args;
    instr->calli.args = args;

    NIR_add_instr(builder, instr);
}

static void NIR_emit_instr_memcpy(NIR_ProcBuilder* builder, Type* type, MemAddr dst, MemAddr src)
{
    Instr* instr = NIR_new_instr(builder->arena, INSTR_MEMCPY);
    instr->memcpy.type = type;
    instr->memcpy.dst = dst;
    instr->memcpy.src = src;

    NIR_add_instr(builder, instr);
}

static void NIR_patch_jmp_target(Instr* jmp_instr, u32 jmp_target)
{
    switch (jmp_instr->kind) {
    case INSTR_JMP:
        *(jmp_instr->jmp.jmp_target) = jmp_target;
        break;
    case INSTR_COND_JMP:
        *(jmp_instr->cond_jmp.jmp_target) = jmp_target;
        break;
    default:
        assert(0);
        break;
    }
}

static u32 NIR_get_jmp_target(NIR_ProcBuilder* builder)
{
    builder->next_instr_is_jmp_target = true;
    return (u32)array_len(builder->curr_proc->as_proc.nir_instrs);
}

static u32* NIR_alloc_jmp_target(NIR_ProcBuilder* builder, u32 init_val)
{
    u32* jmp_target = alloc_type(builder->arena, u32, false);
    *jmp_target = init_val;

    return jmp_target;
}

static NIR_Reg NIR_next_reg(NIR_ProcBuilder* builder)
{
    Symbol* sym = builder->curr_proc;
    NIR_Reg next_reg = sym->as_proc.num_regs++;

    return next_reg;
}

static MemAddr NIR_sym_as_addr(Symbol* sym)
{
    MemAddr addr = {.base_kind = MEM_BASE_SYM, .base.sym = sym};
    return addr;
}

static MemAddr NIR_strlit_as_addr(StrLit* str_lit)
{
    MemAddr addr = {.base_kind = MEM_BASE_STR_LIT, .base.str_lit = str_lit};
    return addr;
}

static void NIR_get_object_addr(NIR_ProcBuilder* builder, MemAddr* dst, Operand* src)
{
    Type* src_type = src->type;

    assert(src_type->kind == TYPE_ARRAY || src_type->kind == TYPE_STRUCT || src_type->kind == TYPE_UNION);

    if (src->kind == NIR_OPERAND_VAR) {
        if (src->sym->is_local) {
            dst->base_kind = MEM_BASE_SYM;
            dst->base.sym = src->sym;
        }
        else {
            NIR_Reg dst_reg = NIR_next_reg(builder);
            NIR_emit_instr_laddr(builder, src_type, dst_reg, NIR_sym_as_addr(src->sym));

            dst->base_kind = MEM_BASE_REG;
            dst->base.reg = dst_reg;
        }

        dst->index_reg = NIR_REG_COUNT;
        dst->disp = 0;
        dst->scale = 0;
    }
    else if (src->kind == NIR_OPERAND_DEREF_ADDR) {
        *dst = src->addr;
    }
    else {
        assert(src->kind == NIR_OPERAND_STR_LIT);

        NIR_Reg dst_reg = NIR_next_reg(builder);
        NIR_emit_instr_laddr(builder, src_type, dst_reg, NIR_strlit_as_addr(src->str_lit));

        dst->base_kind = MEM_BASE_REG;
        dst->base.reg = dst_reg;
        dst->index_reg = NIR_REG_COUNT;
        dst->disp = 0;
        dst->scale = 0;
    }
}

static void NIR_ptr_to_mem_op(NIR_ProcBuilder* builder, NIR_Operand* operand)
{
    assert(operand->type->kind == TYPE_PTR);

    if (operand->kind == NIR_OPERAND_MEM_ADDR) {
        return;
    }

    NIR_Reg base_reg = NIR_REG_COUNT;

    if (operand->kind == NIR_OPERAND_VAR) {
        base_reg = NIR_next_reg(builder);
        NIR_emit_instr_load(builder, operand->type, base_reg, NIR_sym_as_addr(operand->sym));

    }
    else if (operand->kind == NIR_OPERAND_DEREF_ADDR) {
        // Occurs when dereferencing a pointer to a pointer.
        // Ex:
        //     var p  : ^char = "hi";
        //     var pp : ^^char = ^p;
        //     #writeout(*pp + 1, 1); // Happens for expression `*pp + 1`
        NIR_execute_deref(builder, operand);
        base_reg = operand->reg;
    }

    assert(base_reg != NIR_REG_COUNT);

    operand->kind = NIR_OPERAND_MEM_ADDR;
    operand->addr.base_kind = MEM_BASE_REG;
    operand->addr.base.reg = base_reg;
    operand->addr.index_reg = NIR_REG_COUNT;
    operand->addr.scale = 0;
    operand->addr.disp = 0;
}

static void NIR_operand_from_sym(NIR_Operand* op, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR || sym->kind == SYMBOL_PROC);
    op->kind = (sym->kind == SYMBOL_VAR) ? NIR_OPERAND_VAR : NIR_OPERAND_PROC;
    op->type = sym->type;
    op->sym = sym;
}

static void NIR_new_deferred_sc_jmp(NIR_ProcBuilder* builder, NIR_DeferredCmp* cmp, Instr* cmp_instr, bool result, Instr* jmp_instr)
{
    NIR_DeferredJmpcc* new_node = NULL;

    // Pop a node off the freelist.
    if (builder->sc_jmp_freelist) {
        new_node = builder->sc_jmp_freelist;
        builder->sc_jmp_freelist = new_node->next;
    }
    // Create a new node.
    else {
        new_node = alloc_type(builder->tmp_arena, NIR_DeferredJmpcc, true);
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

static void NIR_del_deferred_sc_jmp(NIR_ProcBuilder* builder, NIR_DeferredCmp* cmp, NIR_DeferredJmpcc* prev_jmp, NIR_DeferredJmpcc* jmp)
{
    NIR_DeferredJmpcc* next_jmp = jmp->next;

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

static void NIR_mov_deferred_sc_jmp_list(NIR_DeferredCmp* dst_cmp, NIR_DeferredCmp* src_cmp)
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

static void NIR_copy_sc_jmp(NIR_ProcBuilder* builder, NIR_DeferredJmpcc* dst_jmp, NIR_DeferredJmpcc* src_jmp, bool desired_result)
{
    *dst_jmp = *src_jmp;

    if (dst_jmp->result != desired_result) {
        dst_jmp->cmp.cmp.cond = nir_opposite_cond[dst_jmp->cmp.cmp.cond];
        dst_jmp->result = desired_result;
    }

    if (!dst_jmp->jmp) {
        dst_jmp->jmp = NIR_emit_instr_cond_jmp(builder, NIR_alloc_jmp_target(builder, 0), dst_jmp->cmp.cmp.r);
    }
}

static void NIR_execute_deferred_cmp(NIR_ProcBuilder* builder, NIR_Operand* operand)
{
    assert(operand->kind == NIR_OPERAND_DEFERRED_CMP);

    NIR_DeferredCmp* def_cmp = &operand->cmp;
    NIR_Reg dst_reg = NIR_next_reg(builder);

    bool has_sc_jmps = def_cmp->first_sc_jmp != NULL;
    bool has_final_jmp = def_cmp->final_jmp.jmp != NULL;

    if (!has_sc_jmps && !has_final_jmp) {
        NIR_emit_instr_zext(builder, operand->type, builtin_types[BUILTIN_TYPE_U8].type, dst_reg,
                            def_cmp->final_jmp.cmp.cmp.cond);
    }
    else {
        // Patch short-circuit jumps that jump to the "true" control path.
        for (NIR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next) {
            if (it->result)
                NIR_patch_jmp_target(it->jmp, NIR_get_jmp_target(builder));
        }

        // This is the "true" control path. Move the literal 1 into destination register.
        NIR_emit_instr_limm(builder, operand->type->size, dst_reg, nir_one_imm);

        // Create a jump to skip the false control path.
        NIR_Instr* jmp_skip_false = NIR_emit_instr_jmp(builder, NIR_alloc_jmp_target(builder, 0));

        // Patch short-circuit jumps that jump to the "false" control path.
        for (NIR_DeferredJmpcc* it = def_cmp->first_sc_jmp; it; it = it->next) {
            if (!it->result)
                NIR_patch_jmp_target(it->jmp, NIR_get_jmp_target(builder));
        }

        // Patch final jmp so that it jumps to "false" control path.
        Instr* final_cmp = def_cmp->final_jmp.cmp;

        if (def_cmp->final_jmp.result)
            final_cmp->cmp.cond = nir_opposite_cond[final_cmp->cmp.cond];

        NIR_patch_jmp_target(def_cmp->final_jmp.jmp, NIR_get_jmp_target(builder));

        // This is the "false" control path. Move the literal 0 into destination register.
        NIR_emit_instr_limm(builder, operand->type->size, dst_reg, nir_zero_imm);

        // Patch jump that skips "false" control path.
        NIR_patch_jmp_target(jmp_skip_false, NIR_get_jmp_target(builder));
    }

    operand->kind = NIR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void NIR_execute_deref(NIR_ProcBuilder* builder, NIR_Operand* operand)
{
    assert(operand->kind == NIR_OPERAND_DEREF_ADDR);

    NIR_Reg dst_reg = NIR_next_reg(builder);

    NIR_emit_instr_load(builder, operand->type, dst_reg, operand->addr);

    operand->kind = NIR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void NIR_execute_lea(NIR_ProcBuilder* builder, NIR_Operand* operand)
{
    assert(operand->kind == NIR_OPERAND_MEM_ADDR);

    // The operand currently holds a memory address.
    // This function executes the "load-effective-address" call.
    NIR_MemAddr addr = operand->addr;
    NIR_Reg base_reg = addr.base_kind == MEM_BASE_REG ? addr.base.reg : NIR_REG_COUNT;
    NIR_Reg index_reg = addr.scale ? addr.index_reg : NIR_REG_COUNT;

    bool has_base_reg = base_reg < NIR_REG_COUNT;
    bool has_index_reg = index_reg < NIR_REG_COUNT;
    bool has_disp = addr.disp != 0;

    NIR_Reg dst_reg;

    if (has_base_reg && !has_index_reg && !has_disp) {
        // No need to emit any instructions. Just keep address in base register.
        dst_reg = base_reg;
    }
    else {
        dst_reg = NIR_next_reg(builder);
        NIR_emit_instr_laddr(builder, operand->type, dst_reg, addr);
    }

    operand->kind = NIR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void NIR_op_to_r(NIR_ProcBuilder* builder, NIR_Operand* operand)
{
    switch (operand->kind) {
    case NIR_OPERAND_MEM_ADDR:
        NIR_execute_lea(builder, operand);
        break;
    case NIR_OPERAND_DEREF_ADDR:
        NIR_execute_deref(builder, operand);
        break;
    case NIR_OPERAND_DEFERRED_CMP:
        NIR_execute_deferred_cmp(builder, operand);
        break;
    case NIR_OPERAND_IMM: {
        NIR_Reg reg = NIR_next_reg(builder);

        NIR_emit_instr_limm(builder, operand->type, reg, operand->imm);

        operand->kind = NIR_OPERAND_REG;
        operand->reg = reg;
        break;
    }
    case NIR_OPERAND_PROC:
        NIR_emit_instr_laddr(builder, operand->type, reg, NIR_sym_as_addr(operand->sym));
        break;
    default: {
        NIR_Reg reg = NIR_next_reg(builder);
        NIR_emit_instr_load(builder, operand->type, reg, NIR_sym_as_addr(operand->sym));

        operand->kind = NIR_OPERAND_REG;
        operand->reg = reg;
        break;
    }
    }
}

static void NIR_emit_assign(NIR_ProcBuilder* builder, NIR_Operand* lhs, NIR_Operand* rhs);

// Emit code for initializing an array with an initializer.
//    var a: [11] int = {0, 1, 2, 3};
static void NIR_emit_array_init(NIR_ProcBuilder* builder, NIR_Operand* array_op, NIR_Operand* init_op)
{
    assert(array_op->kind == NIR_OPERAND_VAR || array_op->kind == NIR_OPERAND_DEREF_ADDR);
    assert(init_op->kind == NIR_OPERAND_ARRAY_INIT);
    assert(array_op->type->kind == TYPE_ARRAY);

    Type* arr_type = array_op->type;
    Type* ptr_type = try_array_decay(builder->arena, &builder->type_cache->ptrs, arr_type);
    Type* elem_type = ptr_type->as_ptr.base;

    // Decay array into pointer to the first elem.
    NIR_Operand base_ptr_op = {.kind = NIR_OPERAND_MEM_ADDR, .type = ptr_type};
    NIR_get_object_addr(builder, &base_ptr_op.addr, array_op);

    NIR_ArrayMemberInitializer* initzers = init_op->array_initzer.initzers;
    u64 num_initzers = init_op->array_initzer.num_initzers;
    u64 num_elems = arr_type->as_array.len;

    // Create array of bit flags: 1 bit per element in array.
    // Bit will be set to 1 if the array element has an initializer.
    const int num_bits = sizeof(size_t) * 8;
    size_t num_flags = (num_elems + num_bits - 1) / num_bits;
    size_t* init_flags = alloc_array(builder->tmp_arena, size_t, num_flags, true);

    // Iterate through initializers and: 1. mark element as having an initializer, 2. initialize element.
    for (size_t i = 0; i < num_initzers; i += 1) {
        NIR_ArrayMemberInitializer* initzer = initzers + i;
        size_t elem_index = initzer->index;

        // Mark array element as having an initializer.
        size_t flag_index = elem_index / num_bits;
        size_t bit_index = elem_index % num_bits;
        size_t* flag = init_flags + flag_index;

        *flag |= (1 << bit_index);

        // Initialize array element with value of the initializer.
        NIR_Operand elem_ptr_op = {.kind = NIR_OPERAND_DEREF_ADDR, .type = elem_type, .addr = base_ptr_op.addr};
        elem_ptr_op.addr.disp += elem_type->size * elem_index;

        NIR_emit_assign(builder, &elem_ptr_op, &initzer->op);
    }

    // For each array element, compute the pointer to the corresponding element and assign it
    // an default value if not yet initialized.
    NIR_Operand zero_op = {.kind = NIR_OPERAND_IMM, .type = elem_type, .imm = nir_zero_imm};

    for (u64 elem_index = 0; elem_index < num_elems; elem_index += 1) {
        size_t flag_index = elem_index / num_bits;
        size_t bit_index = elem_index % num_bits;

        // Skip array elements that have been initialized.
        if (init_flags[flag_index] & (1 << bit_index)) {
            continue;
        }

        NIR_Operand elem_ptr_op = {.kind = NIR_OPERAND_DEREF_ADDR, .type = elem_type, .addr = base_ptr_op.addr};
        elem_ptr_op.addr.disp += elem_type->size * elem_index;

        NIR_emit_assign(builder, &elem_ptr_op, &zero_op);
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
static void NIR_emit_array_str_init(NIR_ProcBuilder* builder, NIR_Operand* array_op, NIR_Operand* init_op)
{
    assert(array_op->kind == NIR_OPERAND_VAR || array_op->kind == NIR_OPERAND_DEREF_ADDR);
    assert(init_op->kind == NIR_OPERAND_STR_LIT);
    assert(array_op->type->kind == TYPE_ARRAY);

    Type* arr_type = array_op->type;
    Type* ptr_type = try_array_decay(builder->arena, &builder->type_cache->ptrs, arr_type);
    Type* elem_type = ptr_type->as_ptr.base;
    u64 num_elems = arr_type->as_array.len;

    StrLit* str_lit = init_op->str_lit;
    const char* str = str_lit->str;

    assert((str_lit->len + 1) == num_elems);

    // Decay array into pointer to the first elem.
    NIR_Operand base_ptr_op = {.kind = NIR_OPERAND_MEM_ADDR, .type = ptr_type};
    NIR_get_object_addr(builder, &base_ptr_op.addr, array_op);

    for (u64 elem_index = 0; elem_index < num_elems; elem_index += 1) {
        NIR_Operand char_op = {.kind = NIR_OPERAND_IMM, .type = elem_type, .imm.as_int._u64 = str[elem_index]};

        NIR_Operand elem_ptr_op = {.kind = NIR_OPERAND_DEREF_ADDR, .type = elem_type, .addr = base_ptr_op.addr};
        elem_ptr_op.addr.disp += elem_type->size * elem_index;

        NIR_emit_assign(builder, &elem_ptr_op, &char_op);
    }

    // TODO: Reduce the number of assignment (mov) instructions by initializing
    // multiple elements at a time (one machine word's worth).
}

static void NIR_emit_assign(NIR_ProcBuilder* builder, NIR_Operand* lhs, NIR_Operand* rhs)
{
    MemAddr dst_addr;
    NIR_get_object_addr(builder, &dst_addr, lhs);

    if (rhs->kind == NIR_OPERAND_IMM) {
        NIR_Reg r = NIR_next_reg(builder);

        NIR_emit_instr_limm(builder, rhs->type->size, r, rhs->imm);
        NIR_emit_instr_store(builder, lhs->type, dst_addr, r);
    }
    else if (rhs->kind == NIR_OPERAND_ARRAY_INIT) {
        NIR_emit_array_init(builder, lhs, rhs);
    }
    else if (rhs->kind == NIR_OPERAND_STR_LIT) {
        NIR_emit_array_str_init(builder, lhs, rhs);
    }
    else if (NIR_type_fits_in_reg(rhs->type)) {
        NIR_op_to_r(builder, rhs);
        NIR_emit_instr_store(builder, lhs->type, dst_addr, rhs->reg);
    }
    else {
        MemAddr src_addr;
        NIR_get_object_addr(builder, &src_addr, rhs);
        NIR_emit_instr_memcpy(builder, lhs->type, dst_addr, src_addr);
    }
}

static void NIR_push_scope(NIR_ProcBuilder* builder, Scope* scope)
{
    builder->curr_scope = scope;
}

static void NIR_pop_scope(NIR_ProcBuilder* builder)
{
    builder->curr_scope = builder->curr_scope->parent;
}

//////////////////////////////////////////////////////
//
//         Walk AST and emit NIR instructions
//
//////////////////////////////////////////////////////

static void NIR_emit_stmt(NIR_ProcBuilder* builder, Stmt* stmt, u32* break_target, u32* continue_target);
static void NIR_emit_expr(NIR_ProcBuilder* builder, Expr* expr, NIR_Operand* dst);

static void NIR_emit_expr_ident(NIR_ProcBuilder* builder, ExprIdent* eident, NIR_Operand* dst)
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

    NIR_operand_from_sym(dst, sym);
}

static void NIR_emit_ptr_int_add(NIR_ProcBuilder* builder, NIR_Operand* dst, NIR_Operand* ptr_op, NIR_Operand* int_op, bool add)
{
    u64 base_size = ptr_op->type->as_ptr.base->size;

    NIR_ptr_to_mem_op(builder, ptr_op);

    if (int_op->kind == NIR_OPERAND_IMM) {
        if (add)
            ptr_op->addr.disp += base_size * int_op->imm.as_int._u64;
        else
            ptr_op->addr.disp -= base_size * int_op->imm.as_int._u64;
    }
    else {
        if (ptr_op->addr.scale) {
            NIR_op_to_r(builder, int_op);

            NIR_Reg r = NIR_next_reg(builder);
            NIR_Reg a = ptr_op->addr.index_reg;
            NIR_Reg b = int_op->reg;

            if (add)
                NIR_emit_instr_add(builder, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);
            else
                NIR_emit_instr_sub(builder, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);

            ptr_op->addr.index_reg = r;
        }
        else {
            NIR_op_to_r(builder, int_op);

            if (!add) {
                NIR_Reg a = int_op->reg;

                int_op->reg = NIR_next_reg(builder);
                NIR_emit_instr_neg(builder, int_op->type, int_op->reg, a);
            }

            ptr_op->addr.scale = base_size;
            ptr_op->addr.index_reg = int_op->reg;
        }
    }

    *dst = *ptr_op;
}

static void NIR_emit_binary_cmp(NIR_ProcBuilder* builder, ConditionKind cond_kind, Type* dst_type, NIR_Operand* dst_op,
                                NIR_Operand* left_op, NIR_Operand* right_op)
{
    assert(left_op->type == right_op->type);
    NIR_op_to_r(builder, left_op);
    NIR_op_to_r(builder, right_op);

    NIR_Reg cmp_reg = NIR_next_reg(builder);
    Instr* cmp_instr = NIR_emit_instr_cmp(builder, left_op->type, cond_kind, cmp_reg, left_op->reg, right_op->reg);

    dst_op->type = dst_type;
    dst_op->kind = NIR_OPERAND_DEFERRED_CMP;
    dst_op->cmp.final_jmp.cmp = cmp_instr;
    dst_op->cmp.final_jmp.result = true;
    dst_op->cmp.final_jmp.jmp = NULL;
    dst_op->cmp.first_sc_jmp = NULL;
    dst_op->cmp.last_sc_jmp = NULL;
}

static void NIR_emit_short_circuit_cmp(NIR_ProcBuilder* builder, NIR_Operand* dst_op, ExprBinary* expr)
{
    //
    // NOTE: This procedure will create a deferred comparison containing an array of short-circuit jumps and one final
    // jump. If the left and right subexpressions are themselves deferred comparisons, then they will be merged into
    // this parent expression's deferred comparison. Otherwise, subexpressions that are not deferred comparisons will be
    // compared to zero and converted to either a short-circuit jump (left subexpression) or a final jump (right
    // subexpression).
    //

    dst_op->kind = NIR_OPERAND_DEFERRED_CMP;
    dst_op->type = expr->super.type;

    NIR_Operand left_op = {0};
    NIR_Operand right_op = {0};

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
    NIR_emit_expr(builder, expr->left, &left_op);

    // If the left subexpression is a deferred comparison, merge into this deferred comparison result.
    //
    // Short-circuit jumps from the left subexpression with the same "short-circuit value" are kept as-is.
    //
    // Short-circuit jumps from the left subexpression with the opposite "short-circuit value" are patched
    // with the current instruction index as the jump target and removed. This ensures that short-circuit jumps
    // with the opposite "short-circuit value" are compared to the right subexpression.
    //
    // The left subexpression's final jump is added as a short-circuit jump.
    if (left_op.kind == NIR_OPERAND_DEFERRED_CMP) {
        // Copy list of short-circuit jumps.
        dst_op->cmp.first_sc_jmp = left_op.cmp.first_sc_jmp;
        dst_op->cmp.last_sc_jmp = left_op.cmp.last_sc_jmp;

        // Patch and remove short-circuit jumps with the opposite "short-circuit value".
        NIR_DeferredJmpcc* it = dst_op->cmp.first_sc_jmp;
        NIR_DeferredJmpcc* prev_it = NULL;

        while (it) {
            NIR_DeferredJmpcc* next_it = it->next;

            if (it->result != short_circuit_val) {
                NIR_patch_jmp_target(it->jmp, NIR_get_jmp_target(builder));
                NIR_del_deferred_sc_jmp(builder, &dst_op->cmp, prev_it, it);
            }

            it = next_it;
            prev_it = it;
        }

        // Convert left expression's final jmp to a short-circuit jmp.
        NIR_DeferredJmpcc j;
        NIR_copy_sc_jmp(builder, &j, &left_op.cmp.final_jmp, short_circuit_val);
        NIR_new_deferred_sc_jmp(builder, &dst_op->cmp, j.cmp, j.result, j.jmp);
    }

    // The left subexpression is some computation (not a deferred comparison). Compare the left subexpression to zero
    // and create a short-circuit jmp.
    else {
        NIR_op_to_r(builder, &left_op);

        NIR_Reg imm_reg = NIR_next_reg(builder);
        NIR_emit_instr_limm(builder, left_op.type->size, imm_reg, nir_zero_imm);

        NIR_Reg cmp_reg = NIR_next_reg(builder);
        Instr* cmp_instr = NIR_emit_instr_cmp(builder, left_op.type, short_circuit_cond, cmp_reg, left_op.reg, imm_reg);
        Instr* jmp_instr = NIR_emit_instr_cond_jmp(builder, NIR_alloc_jmp_target(builder, 0), cmp_reg);

        NIR_new_deferred_sc_jmp(builder, &dst_op->cmp, cmp_instr, short_circuit_val, jmp_instr);
    }

    // Emit instructions for the right expression.
    NIR_emit_expr(builder, expr->right, &right_op);

    // If the right subexpression is a deferred comparison, merge into this deferred comparison result.
    // The right subexpression's short-circuit jumps are kept as-is.
    // The right subexpression's final jump is converted to a final jump to the "false" control path.
    if (right_op.kind == NIR_OPERAND_DEFERRED_CMP) {
        // Merge lists of short-circuit jumps.
        NIR_mov_deferred_sc_jmp_list(&dst_op->cmp, &right_op.cmp);

        // Convert the right expression's final jmp into a final jmp to the "false" path.
        NIR_copy_sc_jmp(builder, &dst_op->cmp.final_jmp, &right_op.cmp.final_jmp, false);
    }
    // The right subexpression is some computation (not a deferred comparison). Compare the right subexpression to zero
    // and create a final jump.
    else {
        NIR_op_to_r(builder, &right_op);

        NIR_Reg imm_reg = NIR_next_reg(builder);
        NIR_emit_instr_limm(builder, right_op.type->size, imm_reg, nir_zero_imm);

        NIR_Reg cmp_reg = NIR_next_reg(builder);
        Instr* cmp_instr = NIR_emit_instr_cmp(builder, right_op.type, COND_EQ, cmp_reg, right_op.reg, imm_reg);
        Instr* jmp_instr = NIR_emit_instr_cond_jmp(builder, NIR_alloc_jmp_target(builder, 0), cmp_reg);

        dst_op->cmp.final_jmp.result = false;
        dst_op->cmp.final_jmp.jmp = jmp_instr;
        dst_op->cmp.final_jmp.cmp = cmp_instr;
    }
}

static void NIR_emit_expr_binary(NIR_ProcBuilder* builder, ExprBinary* expr, NIR_Operand* dst)
{
    if (expr->op == TKN_LOGIC_AND || expr->op == TKN_LOGIC_OR) {
        NIR_emit_short_circuit_cmp(builder, dst, expr);
        return;
    }

    Type* result_type = expr->super.type;
    NIR_Operand left = {0};
    NIR_Operand right = {0};

    NIR_emit_expr(builder, expr->left, &left);
    NIR_emit_expr(builder, expr->right, &right);

    switch (expr->op) {
    case TKN_PLUS: {
        bool left_is_ptr = left.type->kind == TYPE_PTR;
        bool right_is_ptr = right.type->kind == TYPE_PTR;

        if (left_is_ptr) {
            NIR_emit_ptr_int_add(builder, dst, &left, &right, true);
        }
        else if (right_is_ptr) {
            NIR_emit_ptr_int_add(builder, dst, &right, &left, true);
        }
        else {
            NIR_op_to_r(builder, &left);
            NIR_op_to_r(builder, &right);

            NIR_Reg dst_reg = NIR_next_reg(builder);

            NIR_emit_instr_add(builder, result_type, dst_reg, left.reg, right.reg);

            dst->kind = NIR_OPERAND_REG;
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

            NIR_op_to_r(builder, &left);
            NIR_op_to_r(builder, &right);
            NIR_Reg dst_reg = NIR_next_reg(builder);

            NIR_emit_instr_sub(builder, result_type, dst_reg, left.reg, right.reg);

            if (base_size_log2 > 0) {

                // Load shift amount into a register.
                Scalar shift_arg = {.as_int._u32 = base_size_log2};
                NIR_Reg shift_reg = NIR_next_reg(builder);
                NIR_emit_instr_limm(builder, 1, shift_reg, shift_arg);

                // Shift result of subtraction by the shift amount.
                NIR_Reg tmp_reg = dst_reg;
                dst_reg = NIR_next_reg(builder);
                NIR_emit_instr_sar(builder, result_type, dst_reg, tmp_reg, shift_reg);
            }

            dst->kind = NIR_OPERAND_REG;
            dst->type = result_type;
            dst->reg = dst_reg;
        }
        // int - int => int
        else {
            NIR_op_to_r(builder, &left);
            NIR_op_to_r(builder, &right);
            NIR_Reg dst_reg = NIR_next_reg(builder);
            NIR_emit_instr_sub(builder, result_type, dst_reg, left.reg, right.reg);

            dst->kind = NIR_OPERAND_REG;
            dst->type = result_type;
            dst->reg = dst_reg;
        }
        break;
    }
    case TKN_ASTERISK: {
        NIR_op_to_r(builder, &left);
        NIR_op_to_r(builder, &right);
        NIR_Reg dst_reg = NIR_next_reg(builder);

        // TODO: Emit a shift instruction if one of the operands is a power-of-two immediate.
        NIR_emit_instr_mul(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = NIR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_DIV: {
        NIR_op_to_r(builder, &left);
        NIR_op_to_r(builder, &right);
        NIR_Reg dst_reg = NIR_next_reg(builder);

        // TODO: Emit a shift instruction if the second operand is a power-of-two immediate.
        NIR_emit_instr_div(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = NIR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_RSHIFT: {
        NIR_op_to_r(builder, &left);
        NIR_op_to_r(builder, &right);
        NIR_Reg dst_reg = NIR_next_reg(builder);

        NIR_emit_instr_sar(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = NIR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_LSHIFT: {
        NIR_op_to_r(builder, &left);
        NIR_op_to_r(builder, &right);
        NIR_Reg dst_reg = NIR_next_reg(builder);

        NIR_emit_instr_shl(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = NIR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_AND: {
        NIR_op_to_r(builder, &left);
        NIR_op_to_r(builder, &right);
        NIR_Reg dst_reg = NIR_next_reg(builder);

        NIR_emit_instr_and(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = NIR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_OR: {
        NIR_op_to_r(builder, &left);
        NIR_op_to_r(builder, &right);
        NIR_Reg dst_reg = NIR_next_reg(builder);

        NIR_emit_instr_or(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = NIR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_CARET: {
        NIR_op_to_r(builder, &left);
        NIR_op_to_r(builder, &right);
        NIR_Reg dst_reg = NIR_next_reg(builder);

        NIR_emit_instr_xor(builder, result_type, dst_reg, left.reg, right.reg);

        dst->kind = NIR_OPERAND_REG;
        dst->type = result_type;
        dst->reg = dst_reg;
        break;
    }
    case TKN_EQ: {
        NIR_emit_binary_cmp(builder, COND_EQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_NOTEQ: {
        NIR_emit_binary_cmp(builder, COND_NEQ, result_type, dst, &left, &right);
        break;
    }
    case TKN_LT: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_LT : COND_U_LT;

        NIR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_LTEQ: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_LTEQ : COND_U_LTEQ;

        NIR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GT: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_GT : COND_U_GT;

        NIR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    case TKN_GTEQ: {
        ConditionKind cond_kind = left.type->as_integer.is_signed ? COND_S_GTEQ : COND_U_GTEQ;

        NIR_emit_binary_cmp(builder, cond_kind, result_type, dst, &left, &right);
        break;
    }
    default:
        assert(0);
        break;
    }
    }
}

static void NIR_emit_int_cast(NIR_ProcBuilder* builder, NIR_Operand* src_op, NIR_Operand* dst_op)
{
    // NOTE:
    // This function treats pointers like integers. The IR currently implements "opaque" pointers, so
    // there are no explicit instructions for converting from one ptr type to another, or converting to/from int/ptr.
    assert(src_op->kind != NIR_OPERAND_IMM); // Should be prevented by resolver.

    // We need the src expression to be in a register.
    NIR_op_to_r(builder, src_op);

    NIR_Reg dst_reg = NIR_REG_COUNT;
    NIR_Reg src_reg = src_op->reg;

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
        dst_reg = NIR_next_reg(builder);
        NIR_emit_instr_trunc(builder, dst_type, src_type, dst_reg, src_reg);
    }
    // Extend (sign or zero) src to larger type.
    else {
        dst_reg = NIR_next_reg(builder);
        InstrKind instr_kind = (src_type->kind == TYPE_INTEGER) && src_type->as_integer.is_signed ? INSTR_SEXT : INSTR_ZEXT;
        NIR_emit_instr_convert(builder, instr_kind, dst_type, src_type, dst_reg, src_reg);
    }

    if (dst_op->type->kind == TYPE_PTR) {
        dst_op->kind = NIR_OPERAND_MEM_ADDR;
        dst_op->addr.base_kind = MEM_BASE_REG;
        dst_op->addr.base.reg = dst_reg;
        dst_op->addr.index_reg = NIR_REG_COUNT;
        dst_op->addr.scale = 0;
        dst_op->addr.disp = 0;
    }
    else {
        dst_op->kind = NIR_OPERAND_REG;
        dst_op->reg = dst_reg;
    }
}

static void NIR_emit_expr_cast(NIR_ProcBuilder* builder, ExprCast* expr_cast, NIR_Operand* dst_op)
{
    // Emit instructions for source expression that will be casted.
    NIR_Operand src_op = {0};
    NIR_emit_expr(builder, expr_cast->expr, &src_op);

    dst_op->type = expr_cast->super.type;

    // TODO: Support floats.
    assert(src_op.type->kind != TYPE_FLOAT);
    assert(dst_op->type->kind != TYPE_FLOAT);
    assert(src_op.type != dst_op->type); // Should be prevented by resolver.

    if (src_op.type->kind == TYPE_ARRAY && dst_op->type->kind == TYPE_PTR) {
        dst_op->kind = NIR_OPERAND_MEM_ADDR;

        NIR_get_object_addr(builder, &dst_op->addr, &src_op);
    }
    else {
        NIR_emit_int_cast(builder, &src_op, dst_op);
    }
}


static bool NIR_type_fits_in_reg(Type* type)
{
    return type->size <= PTR_SIZE;
}

static void NIR_setup_call_ret(NIR_ProcBuilder* builder, ExprCall* expr_call, NIR_Operand* dst_op)
{
    dst_op->type = expr_call->super.type;

    // Allocate register if procedure returns a value.
    if (dst_op->type != builtin_types[BUILTIN_TYPE_VOID].type) {
        if (NIR_type_fits_in_reg(dst_op->type)) {
            dst_op->kind = NIR_OPERAND_REG;
            dst_op->reg = NIR_next_reg(builder);
        }
        else {
            // TODO: Support returning structs
            assert(0);
        }
    }
    else {
        dst_op->kind = NIR_OPERAND_NONE;
        dst_op->reg = NIR_REG_COUNT;
    }
}

static NIR_InstrCallArg* NIR_setup_call_args(NIR_ProcBuilder* builder, ExprCall* expr_call)
{
    u32 num_args = (u32)expr_call->num_args;
    NIR_InstrCallArg* args = alloc_array(builder->arena, NIR_InstrCallArg, num_args, false);

    // Emit instructions for each argument expression and collect the resulting expression values
    // into an `args` array.
    u32 arg_index = 0;
    List* head = &expr_call->args;
    List* it = head->next;

    while (it != head) {
        ProcCallArg* ast_arg = list_entry(it, ProcCallArg, lnode);
        NIR_Operand arg_op = {0};

        NIR_emit_expr(builder, ast_arg->expr, &arg_op);

        if (NIR_type_fits_in_reg(arg_op.type)) {
            NIR_op_to_r(builder, &arg_op);

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

static void NIR_emit_expr_call(NIR_ProcBuilder* builder, ExprCall* expr_call, NIR_Operand* dst_op)
{
    u32 num_args = (u32)expr_call->num_args;
    NIR_InstrCallArg* args = NIR_setup_call_args(builder, expr_call);

    // Emit instructions for the procedure pointer/name.
    NIR_Operand proc_op = {0};
    NIR_emit_expr(builder, expr_call->proc, &proc_op);

    // Allocate register for return value, emit call instruction, and then cleanup.
    if (proc_op.kind == NIR_OPERAND_PROC) {
        // Direct procedure call.
        NIR_setup_call_ret(builder, expr_call, dst_op);
        NIR_emit_instr_call(builder, proc_op.sym, dst_op->reg, num_args, args);
    }
    else {
        // Indirect procedure call through register.
        NIR_op_to_r(builder, &proc_op);
        NIR_setup_call_ret(builder, expr_call, dst_op);
        NIR_emit_instr_call_indirect(builder, proc_op.type, proc_op.reg, dst_op->reg, num_args, args);
    }

    // Mark current procedure as non-leaf.
    builder->curr_proc->as_proc.is_nonleaf = true;
}

static void NIR_emit_expr_compound_lit(NIR_ProcBuilder* builder, ExprCompoundLit* expr, NIR_Operand* dst)
{
    // TODO: Currently only support array initializers.
    assert(expr->super.type->kind == TYPE_ARRAY);
    assert(!expr->typespec);

    u64 initzer_index = 0;
    NIR_ArrayMemberInitializer* ir_initzers = alloc_array(builder->tmp_arena, NIR_ArrayMemberInitializer, expr->num_initzers, true);

    List* head = &expr->initzers;
    List* it = head->next;
    u64 elem_index = 0;

    while (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        NIR_ArrayMemberInitializer* ir_initzer = ir_initzers + initzer_index;

        if (initzer->designator.kind == DESIGNATOR_INDEX) {
            NIR_Operand desig_op = {0};
            NIR_emit_expr(builder, initzer->designator.index, &desig_op);

            assert(desig_op.kind == NIR_OPERAND_IMM);
            elem_index = desig_op.imm.as_int._u64;
        }
        else {
            assert(initzer->designator.kind == DESIGNATOR_NONE);
        }

        ir_initzer->index = elem_index;
        NIR_emit_expr(builder, initzer->init, &ir_initzer->op);

        elem_index += 1;
        initzer_index += 1;
        it = it->next;
    }

    dst->kind = NIR_OPERAND_ARRAY_INIT;
    dst->type = expr->super.type;
    dst->array_initzer.num_initzers = expr->num_initzers;
    dst->array_initzer.initzers = ir_initzers;
}

static void NIR_emit_expr(NIR_ProcBuilder* builder, Expr* expr, NIR_Operand* dst)
{
    if (expr->is_constexpr && expr->is_imm) {
        assert(type_is_scalar(expr->type));
        dst->kind = NIR_OPERAND_IMM;
        dst->type = expr->type;
        dst->imm = expr->imm;

        return;
    }

    switch (expr->kind) {
    case CST_ExprIdent:
        NIR_emit_expr_ident(builder, (ExprIdent*)expr, dst);
        break;
    case CST_ExprCall:
        NIR_emit_expr_call(builder, (ExprCall*)expr, dst);
        break;
    case CST_ExprCast:
        NIR_emit_expr_cast(builder, (ExprCast*)expr, dst);
        break;
    case CST_ExprBinary:
        NIR_emit_expr_binary(builder, (ExprBinary*)expr, dst);
        break;
    case CST_ExprUnary:
        NIR_emit_expr_unary(builder, (ExprUnary*)expr, dst);
        break;
    case CST_ExprIndex:
        NIR_emit_expr_index(builder, (ExprIndex*)expr, dst);
        break;
    case CST_ExprField:
        NIR_emit_expr_field(builder, (ExprField*)expr, dst);
        break;
    case CST_ExprCompoundLit:
        NIR_emit_expr_compound_lit(builder, (ExprCompoundLit*)expr, dst);
        break;
    case CST_ExprStr: {
        ExprStr* expr_str_lit = (ExprStr*)expr;

        dst->kind = NIR_OPERAND_STR_LIT;
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


static void NIR_emit_stmt_block_body(NIR_ProcBuilder* builder, List* stmts, u32* break_target, u32* continue_target)
{
    for (List* it = stmts->next; it != stmts; it = it->next) {
        Stmt* s = list_entry(it, Stmt, lnode);
        NIR_emit_stmt(builder, s, break_target, continue_target);
    }
}

static void NIR_emit_stmt_block(NIR_ProcBuilder* builder, StmtBlock* sblock, u32* break_target, u32* continue_target)
{
    NIR_push_scope(builder, sblock->scope);
    NIR_emit_stmt_block_body(builder, &sblock->stmts, break_target, continue_target);
    NIR_pop_scope(builder);
}

static void NIR_emit_stmt_return(NIR_ProcBuilder* builder, StmtReturn* sret)
{
    NIR_Operand expr_op = {0};
    NIR_emit_expr(builder, sret->expr, &expr_op);
    NIR_op_to_r(builder, &expr_op);

    NIR_emit_instr_ret(builder, expr_op.type, expr_op.reg);
}

static void NIR_emit_stmt_decl(NIR_ProcBuilder* builder, StmtDecl* sdecl)
{
    if (sdecl->decl->kind == CST_DeclConst) {
        return;
    }

    assert(sdecl->decl->kind == CST_DeclVar);

    DeclVar* dvar = (DeclVar*)sdecl->decl;

    if (dvar->init) {
        NIR_Operand rhs_op = {0};
        NIR_Operand lhs_op = {0};

        NIR_emit_expr(builder, dvar->init, &rhs_op);
        NIR_operand_from_sym(&lhs_op, lookup_symbol(builder->curr_scope, dvar->super.name));

        NIR_emit_assign(builder, &lhs_op, &rhs_op);
    }
}

static void NIR_emit_stmt_expr(NIR_ProcBuilder* builder, StmtExpr* sexpr)
{
    NIR_Operand expr_op = {0};
    NIR_emit_expr(builder, sexpr->expr, &expr_op);

    // Actually execute any deferred operations.
    switch (expr_op.kind) {
    case NIR_OPERAND_DEREF_ADDR:
        NIR_execute_deref(builder, &expr_op);
        break;
    case NIR_OPERAND_DEFERRED_CMP:
        NIR_execute_deferred_cmp(builder, &expr_op);
        break;
    case NIR_OPERAND_MEM_ADDR:
        NIR_execute_lea(builder, &expr_op);
        break;
    default:
        break;
    }
}

static void NIR_emit_stmt_expr_assign(NIR_ProcBuilder* builder, StmtExprAssign* stmt)
{
    switch (stmt->op_assign) {
    case TKN_ASSIGN: {
        NIR_Operand lhs_op = {0};
        NIR_Operand rhs_op = {0};

        NIR_emit_expr(builder, stmt->left, &lhs_op);
        NIR_emit_expr(builder, stmt->right, &rhs_op);

        NIR_emit_assign(builder, &lhs_op, &rhs_op);
        break;
    }
    default:
        assert(!"Unsupported assignment operator in IR generation");
        break;
    }
}

static void NIR_emit_stmt(NIR_ProcBuilder* builder, Stmt* stmt, u32* break_target, u32* continue_target)
{
    switch (stmt->kind) {
    case CST_StmtBlock:
        NIR_emit_stmt_block(builder, (StmtBlock*)stmt, break_target, continue_target);
        break;
    case CST_StmtReturn:
        NIR_emit_stmt_return(builder, (StmtReturn*)stmt);
        break;
    case CST_StmtDecl:
        NIR_emit_stmt_decl(builder, (StmtDecl*)stmt);
        break;
    case CST_StmtExpr:
        NIR_emit_stmt_expr(builder, (StmtExpr*)stmt);
        break;
    case CST_StmtExprAssign:
        NIR_emit_stmt_expr_assign(builder, (StmtExprAssign*)stmt);
        break;
    case CST_StmtIf:
        //NIR_emit_stmt_if(builder, (StmtIf*)stmt, break_target, continue_target);
        assert(0);
        break;
    case CST_StmtWhile:
        //NIR_emit_stmt_while(builder, (StmtWhile*)stmt);
        assert(0);
        break;
    case CST_StmtDoWhile:
        //NIR_emit_stmt_do_while(builder, (StmtDoWhile*)stmt);
        assert(0);
        break;
    case CST_StmtBreak:
        NIR_emit_instr_jmp(builder, break_target);
        break;
    case CST_StmtContinue:
        NIR_emit_instr_jmp(builder, continue_target);
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

static void NIR_build_proc(NIR_ProcBuilder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    if (dproc->is_incomplete) {
        return;
    }

    // Set procedure as the current scope.
    NIR_push_scope(builder, dproc->scope);
    builder->curr_proc = sym;

    sym->as_proc.nir_instrs = array_create(builder->arena, Instr*, 32);

    NIR_emit_stmt_block_body(builder, &dproc->stmts, NULL, NULL);

    // If proc doesn't have explicit returns, add one at the end.
    // NOTE: This should only apply to procs that return void. The resolver
    // will catch other cases.
    if (!dproc->returns) {
        assert(sym->type->as_proc.ret == builtin_types[BUILTIN_TYPE_VOID].type);
        NIR_emit_instr_ret(builder, builtin_types[BUILTIN_TYPE_VOID].type, NIR_REG_COUNT);
    }

    NIR_pop_scope(builder);
    builder->curr_proc = NULL;
}

static void NIR_build_procs(Allocator* arena, Allocator* tmp_arena, BucketList* procs, TypeCache* type_cache)
{
    NIR_ProcBuilder builder =
        {.arena = arena, .tmp_arena = tmp_arena, .type_cache = type_cache, .curr_proc = NULL, .curr_scope = NULL};

    AllocatorState tmp_mem_state = allocator_get_state(builder.tmp_arena);

    // Iterate through all procedures and generate IR instructions.
    size_t num_procs = procs->num_elems;

    for (size_t i = 0; i < num_procs; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_PROC);

        NIR_build_proc(&builder, sym);
    }

    allocator_restore_state(tmp_mem_state);
}
