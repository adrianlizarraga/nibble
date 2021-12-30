
// r = a + b
/*
    r2 = r0 + r1

    mov r2, r0
    add r2, r1

    r2 = r0 - r1

    mov r2, r0
    sub r2, r1

    r2 = r0 * r1

    mov r2, r0
    imul r2, r1

    r2 = r0 / r1

    if size >= 2 bytes

    mov _ax, r0 // reserve rax and rdx (if size >= 2)
    cqo ; sign extend into _dx
    div r1
    mov r2, _ax
    

*/

typedef struct X64_RegRange {
    u32 start;
    u32 end;
    X64_Reg reg;
    bool locked;
} X64_RegRange;

typedef struct X64_LLIRBuilder {
    Allocator* arena;

    X64_Instr** instrs; // Stretchy buf
    X64_RegRange* llir_ranges; // Stretchy buf
    u32* reg_map; // Map IR reg -> LLIR reg; size: num_iregs

    // Disjoint Set Union data structure for register renaming/aliasing.
    u32* llir_aliases; // Root alias node for each llir reg. size: num_llirregs
    u32* llir_sizes;   // Size for each llir reg aliasing set. size: num_llirregs
} X64_LLIRBuilder;

static void X64_init_llir_builder(X64_LLIRBuilder* builder, Allocator* arena, u32 num_irregs)
{
    builder->arena = arena;
    builder->instrs = array_create(arena, X64_Instr*, 32);
    builder->llir_ranges = array_create(arena, X64_RegRange, 16);
    builder->llir_aliases = array_create(arena, u32, 16);
    builder->llir_sizes = array_create(arena, u32, 16);

    builder->reg_map = alloc_array(arena, u32, num_irregs, false);
    memset(builder->reg_map, 0xFF, num_iiregs * sizeof(u32));
}

static void X64_end_reg_range(X64_LLIRBuilder* builder, u32 r)
{
    X64_RegRange* range = builder->llir_ranges + r;
    assert(!range->locked);
    range->end = array_len(builder->instrs) - 1;
    range->locked = true;
}

static void X64_merge_ranges(X64_RegRange* dst_range, X64_RegRange* src_range)
{
    dst_range->start = dst_range->start <= src_range->start ? dst_range->start : src_range->start;
    dst_range->end = dst_range->end >= src_range->end ? dst_range->end : src_range->end;

    if (src_range->reg != X64_REG_COUNT) {
        assert(dst_range->reg == X64_REG_COUNT);
        dst_range->reg = src_range->reg;
    }

    dst_range->locked = dst_range->locked || src_range->locked;
}

static u32 X64_find_alias_reg(X64_LLIRBuilder* builder, u32 r)
{
    u32* roots = builder->llir_aliases;

    while (roots[r] != r) {
        u32 next_r = roots[r];
        roots[r] = roots[next_r];
        r = next_r;
    }

    return r;
}

static void X64_alias_llir_regs(X64_LLIRBuilder* builder, u32 u, u32 v)
{
    u32 root_u = X64_find_alias_reg(builder, u);
    u32 root_v = X64_find_alias_reg(builder, v);

    if (root_u == root_v) {
        return;
    }

    u32* roots = builder->llir_aliases;
    u32* sizes = builder->llir_sizes;

    if (sizes[root_u] > sizes[root_v]) {
        roots[root_v] = root_u;
        sizes[root_u] += sizes[root_v];

        X64_RegRange* ranges = builder->llir_ranges;
        X64_merge_ranges(ranges + root_u, ranges + root_v);
    }
    else {
        roots[root_u] = root_v;
        sizes[root_v] += sizes[root_u];

        X64_RegRange* ranges = builder->llir_ranges;
        X64_merge_ranges(ranges + root_v, ranges + root_u);
    }
}

static u32 X64_next_llir_reg(X64_LLIRBuilder* builder)
{
    size_t next_ip = array_len(builder->instrs);

    array_push(builder->llir_ranges, (X64_RegRange){.start = next_ip, .end = next_ip, .reg = X64_REG_COUNT});

    u32 next_reg = array_len(builder->llir_ranges) - 1;
    assert(next_reg < (u32)-1);

    array_push(builder->llir_aliases, next_reg);
    array_push(builder->llir_sizes, 1);

    return next_reg;
}

static u32 X64_def_llir_reg(X64_LLIRBuilder* builder, u32 irreg)
{
    assert(builder->reg_map[iireg] == (u32)-1);
    u32 result = X64_next_llir_reg(builder);
    builder->reg_map[irreg] = result;

    return result;
}

static u32 X64_get_llir_reg(X64_LLIRBuilder* builder, u32 irreg)
{
    size_t next_ip = array_len(builder->instrs);

    u32 result = builder->reg_map[irreg];

    assert(result != (u32)-1);

    result = X64_find_alias_reg(builder, result);

    X64_RegRange* range = &builder->llir_ranges[result];
    assert(!range->locked);

    range->end = next_ip;

    return result;
}

static u32 X64_def_phys_reg(X64_LLIRBuilder* builder, X64_Reg phys_reg)
{
    u32 result = X64_next_llir_reg(builder);
    X64_RegRange* range = &builder->llir_ranges[result];

    range->reg = phys_reg;
}

static void X64_emit_llir_instr(X64_LLIRBuilder* builder, Instr* ir_instr)
{
    static X64_InstrKind binary_kind[] = {
        [INSTR_ADD] = X64_INSTR_ADD_R_R,
        [INSTR_SUB] = X64_INSTR_SUB_R_R,
        [INSTR_MUL] = X64_INSTR_IMUL_R_R,
        [INSTR_AND] = X64_INSTR_AND_R_R,
        [INSTR_OR]  = X64_INSTR_OR_R_R,
        [INSTR_XOR] = X64_INSTR_XOR_R_R
    };

    static X64_InstrKind div_kind[] = {
        [INSTR_UDIV] = X64_INSTR_DIV,
        [INSTR_SDIV] = X64_INSTR_IDIV
    };

    static X64_InstrKind shift_kind[] = {
        [INSTR_SHL] = X64_INSTR_SHL,
        [INSTR_SAR] = X64_INSTR_SAR
    };

    static X64_InstrKind unary_kind[] = {
        [INSTR_NEG] = X64_INSTR_NEG,
        [INSTR_NOT] = X64_INSTR_NOT
    };

    switch (ir_instr->kind) {
    case INSTR_ADD:
    case INSTR_SUB:
    case INSTR_MUL:
    case INSTR_AND:
    case INSTR_OR:
    case INSTR_XOR: {
        // EX: r = a + b
        //
        // mov r, a
        // add r, b
        size_t size = ir_instr->binary.type->size;

        u32 r = X64_def_llir_reg(builder, ir_instr->binary.r);
        u32 a = X64_get_llir_reg(builder, ir_instr->binary.a);
        u32 b = X64_get_llir_reg(builder, ir_instr->binary.b);

        X64_emit_mov_r_r(builder, size, r, a);
        X64_emit_binary_r_r(builder, binary_kind[ir_instr->kind], size, r, b);

        break;
    }
    case INSTR_UDIV:
    case INSTR_SDIV: {

        // EX: r = a / b
        //
        // mov _ax, a ; Only if `a` is not already in `_ax`
        // cqo        ; sign extend into `_dx` if size >= 2 bytes
        // div b
        // mov r, _ax

        size_t size = ir_instr->binary.type->size;
        bool uses_dx = size >= 2;

        // mov _ax, a
        u32 a = X64_get_llir_reg(builder, ir_instr->binary.a);
        u32 ax = X64_def_phys_reg(builder, X64_RAX);

        X64_emit_mov_r_r(builder, size, ax, a);

        // cqo
        u32 dx;
        if (uses_dx) { // Reserve rdx
            dx = X64_def_phys_reg(builder, X64_RDX);
            X64_emit_sext_ax_to_dx(builder, size);
        }

        // div b
        u32 b = X64_get_llir_reg(builder, ir_instr->binary.b);
        X64_emit_div_r(builder, div_kind[ir_instr->kind], size, b);

        if (uses_dx) {
            X64_end_reg_range(builder, dx);
        }

        // mov r, _ax
        u32 r = X64_def_llir_reg(builder, ir_instr->binary.r);
        X64_emit_mov_r_r(builder, size, r, ax);

        X64_end_reg_range(builder, ax);
        break;
    }
    case INSTR_SAR:
    case INSTR_SHL: {
        size_t size = ir_instr->binary.type->size;

        // mov r, a
        u32 r = X64_def_llir_reg(builder, ir_instr->binary.r);
        u32 a = X64_get_llir_reg(builder, ir_instr->binary.a);
        X64_emit_mov_r_r(builder, size, r, a);

        // mov _cx, b
        u32 b = X64_get_llir_reg(builder, ir_instr->binary.b);
        u32 cx = X64_def_phys_reg(builder, X64_RCX);

        X64_emit_mov_r_r(builder, size, cx, b);

        // shl r, _cx
        X64_emit_shift_r_r(builder, shift_kind[ir_instr->kind], size, r, cx);

        X64_end_reg_range(builder, cx);

        break;
    }
    case INSTR_NEG:
    case INSTR_NOT: {
        // EX: r = ~neg
        //
        // mov r, a
        // neg r
        size_t size = ir_instr->unary.type->size;

        u32 r = X64_def_llir_reg(builder, ir_instr->unary.r);
        u32 a = X64_get_llir_reg(builder, ir_instr->unary.a);

        X64_emit_mov_r_r(builder, size, r, a);
        X64_emit_unary_r_r(builder, unary_kind[ir_instr->kind], size, r);
        break;
    }
    default:
        NIBBLE_FATAL_EXIT("[INTERNAL ERROR]: Unable to convert IR instruction %d to an X64 LLIR instruction", ir_instr->kind);
        break;
    }
}



