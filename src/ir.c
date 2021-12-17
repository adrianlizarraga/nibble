#include "ir.h"

#define NIR_REG_COUNT 0xFFFFFFFF

typedef struct NIR_ProcBuilder {
    Allocator* arena;
    Allocator* tmp_arena;
    TypeCache* type_cache;
    Symbol* curr_proc;
    Scope* curr_scope;

    bool next_instr_is_jmp_target;
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

static void NIR_operand_from_sym(IR_Operand* op, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR || sym->kind == SYMBOL_PROC);
    op->kind = (sym->kind == SYMBOL_VAR) ? NIR_OPERAND_VAR : NIR_OPERAND_PROC;
    op->type = sym->type;
    op->sym = sym;
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
    NIR_Operand zero_op = {.kind = IR_OPERAND_IMM, .type = elem_type, .imm = nir_zero_imm};

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
    else if (rhs->kind == IR_OPERAND_ARRAY_INIT) {
        NIR_emit_array_init(builder, lhs, rhs);
    }
    else if (rhs->kind == IR_OPERAND_STR_LIT) {
        NIR_emit_array_str_init(builder, lhs, rhs);
    }
    else if (IR_type_fits_in_reg(rhs->type)) {
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
            IR_op_to_r(builder, int_op);

            NIR_Reg r = NIR_next_reg(builder);
            NIR_Reg a = ptr_op->addr.index_reg;
            NIR_Reg b = int_op->reg;

            if (add)
                IR_emit_instr_add(builder, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);
            else
                IR_emit_instr_sub(builder, builtin_types[BUILTIN_TYPE_S64].type, r, a, b);

            ptr_op->addr.index_reg = r;
        }
        else {
            NIR_op_to_r(builder, int_op);

            if (!add) {
                NIR_Reg a = int_op->reg;

                int_op->reg = NIR_next_reg(builder);
                IR_emit_instr_neg(builder, int_op->type, int_op->reg, a);
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
    // TODO: Left off here!
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
