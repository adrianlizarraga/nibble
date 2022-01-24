#include "bytecode.h"
#include "x64_gen/lir.h"

static X64_InstrKind binary_kind[] = {
    [INSTR_ADD] = X64_INSTR_ADD_R_R,
    [INSTR_SUB] = X64_INSTR_SUB_R_R,
    [INSTR_MUL] = X64_INSTR_IMUL_R_R,
    [INSTR_AND] = X64_INSTR_AND_R_R,
    [INSTR_OR]  = X64_INSTR_OR_R_R,
    [INSTR_XOR] = X64_INSTR_XOR_R_R
};

static X64_InstrKind binary_r_i_kind[] = {
    [INSTR_ADD] = X64_INSTR_ADD_R_I,
    [INSTR_SUB] = X64_INSTR_SUB_R_I,
    [INSTR_MUL] = X64_INSTR_IMUL_R_I,
    [INSTR_AND] = X64_INSTR_AND_R_I,
    [INSTR_OR]  = X64_INSTR_OR_R_I,
    [INSTR_XOR] = X64_INSTR_XOR_R_I
};

static X64_InstrKind div_kind[] = {
    [INSTR_UDIV] = X64_INSTR_DIV,
    [INSTR_SDIV] = X64_INSTR_IDIV
};

static X64_InstrKind shift_kind[] = {
    [INSTR_SHL] = X64_INSTR_SHL_R_R,
    [INSTR_SAR] = X64_INSTR_SAR_R_R
};

static X64_InstrKind shift_r_i_kind[] = {
    [INSTR_SHL] = X64_INSTR_SHL_R_I,
    [INSTR_SAR] = X64_INSTR_SAR_R_I
};

static X64_InstrKind unary_kind[] = {
    [INSTR_NEG] = X64_INSTR_NEG,
    [INSTR_NOT] = X64_INSTR_NOT
};

static X64_InstrKind convert_kind[] = {
    [INSTR_TRUNC] = X64_INSTR_MOV_R_R,
    [INSTR_SEXT] = X64_INSTR_MOVSX_R_R,
    [INSTR_ZEXT] = X64_INSTR_MOVZX_R_R
};

static void X64_init_lir_builder(X64_LIRBuilder* builder, Allocator* arena, u32 num_iregs)
{
    builder->arena = arena;
    builder->num_regs = 0;
    builder->num_instrs = 0;
    builder->lreg_aliases = array_create(arena, u32, 16);
    builder->lreg_sizes = array_create(arena, u32, 16);

    builder->reg_map = alloc_array(arena, u32, num_iregs, false);
    memset(builder->reg_map, 0xFF, num_iregs * sizeof(u32));
}

u32 X64_find_alias_reg(X64_LIRBuilder* builder, u32 r)
{
    u32* roots = builder->lreg_aliases;

    while (roots[r] != r) {
        u32 next_r = roots[r];
        roots[r] = roots[next_r];
        r = next_r;
    }

    return r;
}

static void X64_alias_lir_regs(X64_LIRBuilder* builder, u32 u, u32 v)
{
    u32 root_u = X64_find_alias_reg(builder, u);
    u32 root_v = X64_find_alias_reg(builder, v);

    if (root_u == root_v) {
        return;
    }

    u32* roots = builder->lreg_aliases;
    u32* sizes = builder->lreg_sizes;

    if (sizes[root_u] > sizes[root_v]) {
        roots[root_v] = root_u;
        sizes[root_u] += sizes[root_v];
    }
    else {
        roots[root_u] = root_v;
        sizes[root_v] += sizes[root_u];
    }
}

static u32 X64_next_lir_reg(X64_LIRBuilder* builder)
{
    u32 next_reg = builder->num_regs++;

    array_push(builder->lreg_aliases, next_reg);
    array_push(builder->lreg_sizes, 1);

    return next_reg;
}

static u32 X64_def_lir_reg(X64_LIRBuilder* builder, u32 ireg)
{
    assert(builder->reg_map[ireg] == (u32)-1);
    u32 result = X64_next_lir_reg(builder);
    builder->reg_map[ireg] = result;

    return result;
}

static u32 X64_get_lir_reg(X64_LIRBuilder* builder, u32 ireg)
{
    u32 result = builder->reg_map[ireg];

    assert(result != (u32)-1);

    return X64_find_alias_reg(builder, result);
}

static u32 X64_get_lir_phys_reg(X64_LIRBuilder* builder, X64_Reg phys_reg)
{
    return builder->lreg_phys[phys_reg];
}

static void X64_get_lir_addr(X64_LIRBuilder* builder, X64_MemAddr* dst, MemAddr* src)
{
    bool has_base = src->base_kind != MEM_BASE_NONE;
    bool has_index = src->scale && (src->index_reg < IR_REG_COUNT);
    assert(has_base || has_index);

    if (has_base) {
        if (src->base_kind == MEM_BASE_STR_LIT) {
            dst->kind = X64_ADDR_STR_LIT;
            dst->str_lit = src->base.str_lit;

            return;
        }

        if (src->base_kind == MEM_BASE_SYM) {
            Symbol* sym = src->base.sym;

            // Early exit for global variable addresses.
            if (!sym->is_local) {
                dst->kind = X64_ADDR_GLOBAL;
                dst->global = sym;

                return;
            }

            dst->kind = X64_ADDR_LOCAL;
            dst->local.base_reg = builder->lreg_phys[X64_RBP];
            dst->local.disp = src->disp + sym->as_var.offset;
            dst->local.scale = src->scale;
        }
        else {
            u32 base_reg = X64_get_lir_reg(builder, src->base.reg);
            //X64_force_any_reg(builder, base_reg);

            dst->kind = X64_ADDR_LOCAL;
            dst->local.base_reg = base_reg;
            dst->local.disp = src->disp;
            dst->local.scale = src->scale;
        }

        if (has_index) {
            u32 index_reg = X64_get_lir_reg(builder, src->index_reg);
            //X64_force_any_reg(builder, index_reg);

            dst->local.index_reg = index_reg;
        }
        else {
            dst->local.index_reg = (u32)-1;
        }
    }
    else {
        u32 index_reg = X64_get_lir_reg(builder, src->index_reg);
        //X64_force_any_reg(builder, index_reg);

        dst->kind = X64_ADDR_LOCAL;
        dst->local.base_reg = (u32)-1;
        dst->local.disp = src->disp;
        dst->local.scale = src->scale;
        dst->local.index_reg = index_reg;
    }
}

static X64_StackArgsInfo X64_linux_convert_call_args(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 num_args, InstrCallArg* args,
                                                     X64_InstrCallArg* x64_args)
{
    X64_StackArgsInfo stack_info = {0};
    u32 arg_reg_index = 0;

    for (u32 i = 0; i < num_args; i++) {
        InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;
        X64_InstrCallArg* arg_info = x64_args + i;

        assert(arg_size <= X64_MAX_INT_REG_SIZE); // TODO: Support structs

        arg_info->type = arg->type;
        arg_info->loc = X64_get_lir_reg(builder, arg->loc);

        if (arg_reg_index >= x64_target.num_arg_regs) {
            arg_info->in_reg = false;
            arg_info->sp_offset = stack_info.size;

            stack_info.size += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }
        else {
            X64_Reg phys_reg = x64_target.arg_regs[arg_reg_index++];
            X64_emit_instr_mov_r_r(builder, xbblock, arg_size, X64_get_lir_phys_reg(builder, phys_reg), arg_info->loc);

            arg_info->in_reg = true;
        }
    }

    return stack_info;
}

static X64_StackArgsInfo X64_windows_convert_call_args(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 num_args, InstrCallArg* args,
                                                       X64_InstrCallArg* x64_args)
{
    X64_StackArgsInfo stack_info = {.size = X64_WINDOWS_SHADOW_SPACE, .offset = X64_WINDOWS_SHADOW_SPACE};

    for (u32 i = 0; i < num_args; i++) {
        InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;
        X64_InstrCallArg* arg_info = x64_args + i;

        assert(arg_size <= X64_MAX_INT_REG_SIZE); // TODO: Support structs
        arg_info->type = arg->type;
        arg_info->loc = X64_get_lir_reg(builder, arg->loc);

        if (i >= x64_target.num_arg_regs) {
            arg_info->in_reg = false;
            arg_info->sp_offset = stack_info.size;

            stack_info.size += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }
        else {
            X64_Reg phys_reg = x64_target.arg_regs[i];
            X64_emit_instr_mov_r_r(builder, xbblock, arg_size, X64_get_lir_phys_reg(builder, phys_reg), arg_info->loc);

            arg_info->in_reg = true;
        }
    }

    return stack_info;
}

static X64_StackArgsInfo X64_convert_call_args(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 num_args, InstrCallArg* args,
                                               X64_InstrCallArg* x64_args)
{
    if (x64_target.os == OS_LINUX) {
        return X64_linux_convert_call_args(builder, xbblock, num_args, args, x64_args);
    }
    else {
        return X64_windows_convert_call_args(builder, xbblock, num_args, args, x64_args);
    }
}

static bool X64_try_combine_limm(X64_LIRBuilder* builder, X64_BBlock* xbblock, Instr** p_ir_instr)
{
    static const u32 INSTR_IS_BINARY = 0x1;
    static const u32 INSTR_IS_COMM = 0x2;
    static const u32 INSTR_IS_SHIFT = 0x4;
    static u32 instr_kind_flags[INSTR_KIND_COUNT] = {
        [INSTR_ADD] = INSTR_IS_BINARY | INSTR_IS_COMM,
        [INSTR_SUB] = INSTR_IS_BINARY,
        [INSTR_MUL] = INSTR_IS_BINARY | INSTR_IS_COMM,
        [INSTR_AND] = INSTR_IS_BINARY | INSTR_IS_COMM,
        [INSTR_OR]  = INSTR_IS_BINARY | INSTR_IS_COMM,
        [INSTR_XOR] = INSTR_IS_BINARY | INSTR_IS_COMM,
        [INSTR_SHL] = INSTR_IS_SHIFT,
        [INSTR_SAR] = INSTR_IS_SHIFT,
    };

    // Look for an instruction to combine with
    Instr* ir_instr = *p_ir_instr;
    Instr* t_instr = ir_instr->next;

    if (!t_instr) {
        return false;
    }

    IR_Reg imm_reg = ir_instr->limm.r;
    Scalar imm = ir_instr->limm.imm;

    InstrKind t_kind = t_instr->kind;
    u32 flags = instr_kind_flags[t_kind];

    bool is_binary = flags & INSTR_IS_BINARY;
    bool is_comm = flags & INSTR_IS_COMM;
    bool is_shift = flags & INSTR_IS_SHIFT;
    bool is_store = t_kind == INSTR_STORE;

    if (is_binary) {
        u32 a = (u32)-1; // Will be set to the register with the non-immediate operand

        // Check if can combine with commutative instructions: ADD, MUL, AND, OR, XOR
        if (is_comm) {
            if (t_instr->binary.b == imm_reg) {
                a = X64_get_lir_reg(builder, t_instr->binary.a);
            }
            else if (t_instr->binary.a == imm_reg) {
                a = X64_get_lir_reg(builder, t_instr->binary.b);
            }
        }
        // Check if can combine with SUB
        else if (t_instr->binary.b == imm_reg) {
            a = X64_get_lir_reg(builder, t_instr->binary.a);
        }

        if (a != (u32)-1) {
            // EX: r = a + 12
            //
            //     mov r, a
            //     add r, 12
            size_t size = t_instr->binary.type->size;

            u32 r = X64_def_lir_reg(builder, t_instr->binary.r);

            X64_emit_instr_mov_r_r(builder, xbblock, size, r, a);
            X64_emit_instr_binary_r_i(builder, xbblock, binary_r_i_kind[t_kind], size, r, imm);

            *p_ir_instr = t_instr;
            return true;
        }
    }
    else if (is_shift) {
        if (t_instr->shift.b == imm_reg) {
            // EX: r = a << 1
            //
            //     mov r, a
            //     shl r, 1
            size_t size = t_instr->shift.type->size;
            
            u32 r = X64_def_lir_reg(builder, t_instr->shift.r);
            u32 a = X64_get_lir_reg(builder, t_instr->shift.a);

            X64_emit_instr_mov_r_r(builder, xbblock, size, r, a);
            X64_emit_instr_shift_r_i(builder, xbblock, shift_r_i_kind[t_kind], size, r, imm);

            *p_ir_instr = t_instr;
            return true;
        }
    }
    else if (is_store) {
        if (t_instr->store.a == imm_reg) {
            // EX: $addr = 1
            //
            //     mov [..addr], 1
            size_t size = t_instr->store.type->size;

            X64_MemAddr addr;
            X64_get_lir_addr(builder, &addr, &t_instr->store.addr);

            X64_emit_instr_mov_m_i(builder, xbblock, size, addr, imm);
            *p_ir_instr = t_instr;
            return true;
        }
    }

    return false;
}

static Instr* X64_convert_ir_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, Instr* ir_instr)
{
    Instr* next_instr = ir_instr->next;

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

        u32 r = X64_def_lir_reg(builder, ir_instr->binary.r);
        u32 a = X64_get_lir_reg(builder, ir_instr->binary.a);

        X64_emit_instr_mov_r_r(builder, xbblock, size, r, a);

        u32 b = X64_get_lir_reg(builder, ir_instr->binary.b);
        X64_emit_instr_binary_r_r(builder, xbblock, binary_kind[ir_instr->kind], size, r, b);
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

        u32 a = X64_get_lir_reg(builder, ir_instr->binary.a);
        u32 ax = X64_get_lir_phys_reg(builder, X64_RAX);

        // mov _ax, a
        X64_emit_instr_mov_r_r(builder, xbblock, size, ax, a);

        // cqo
        if (uses_dx) { // Reserve rdx
            X64_emit_instr_sext_ax_to_dx(builder, xbblock, size);
        }

        // div b
        u32 b = X64_get_lir_reg(builder, ir_instr->binary.b);
        X64_emit_instr_div(builder, xbblock, div_kind[ir_instr->kind], size, b);

        // mov r, a
        u32 r = X64_def_lir_reg(builder, ir_instr->binary.r);
        X64_emit_instr_mov_r_r(builder, xbblock, size, r, a);
        break;
    }
    case INSTR_SAR:
    case INSTR_SHL: {
        size_t size = ir_instr->shift.type->size;

        // mov r, a
        u32 r = X64_def_lir_reg(builder, ir_instr->shift.r);
        u32 a = X64_get_lir_reg(builder, ir_instr->shift.a);
        X64_emit_instr_mov_r_r(builder, xbblock, size, r, a);

        // mov _cx, b
        u32 b = X64_get_lir_reg(builder, ir_instr->shift.b);
        u32 cx = X64_get_lir_phys_reg(builder, X64_RCX);
        X64_emit_instr_mov_r_r(builder, xbblock, size, cx, b);

        // shift r, _cx
        X64_emit_instr_shift_r_r(builder, xbblock, shift_kind[ir_instr->kind], size, r, cx);
        break;
    }
    case INSTR_NEG:
    case INSTR_NOT: {
        // EX: r = ~neg
        //
        // mov r, a
        // neg r
        size_t size = ir_instr->unary.type->size;

        u32 r = X64_def_lir_reg(builder, ir_instr->unary.r);
        u32 a = X64_get_lir_reg(builder, ir_instr->unary.a);

        X64_emit_instr_mov_r_r(builder, xbblock, size, r, a);
        X64_emit_instr_unary(builder, xbblock, unary_kind[ir_instr->kind], size, r);
        break;
    }
    case INSTR_TRUNC: {
        size_t dst_size = ir_instr->convert.dst_type->size;

        u32 r = X64_def_lir_reg(builder, ir_instr->convert.r);
        u32 a = X64_get_lir_reg(builder, ir_instr->convert.a);

        X64_emit_instr_mov_r_r(builder, xbblock, dst_size, r, a);
        break;
    }
    case INSTR_ZEXT:
    case INSTR_SEXT: {
        // EX: r = sext(a)
        //
        // movsx r, a

        size_t dst_size = ir_instr->convert.dst_type->size;
        size_t src_size = ir_instr->convert.src_type->size;

        u32 r = X64_def_lir_reg(builder, ir_instr->convert.r);
        u32 a = X64_get_lir_reg(builder, ir_instr->convert.a);

        X64_emit_instr_convert_r_r(builder, xbblock, convert_kind[ir_instr->kind], dst_size, r, src_size, a);
        break;
    }
    case INSTR_LIMM: {
        // EX: r = 10
        //
        // mov r, 10

        size_t size = ir_instr->limm.type->size;

        if (!X64_try_combine_limm(builder, xbblock, &ir_instr)) {
            u32 r = X64_def_lir_reg(builder, ir_instr->limm.r);
            
            X64_emit_instr_mov_r_i(builder, xbblock, size, r, ir_instr->limm.imm);
        }

        break;
    }
    case INSTR_LOAD: {
        // EX: r = load(base + scale * index + disp)
        //
        // mov r, [base + scale * index + disp]

        size_t size = ir_instr->load.type->size;

        X64_MemAddr addr;
        X64_get_lir_addr(builder, &addr, &ir_instr->load.addr);

        u32 r = X64_def_lir_reg(builder, ir_instr->load.r);
        X64_emit_instr_mov_r_m(builder, xbblock, size, r, addr);
        break;
    }
    case INSTR_LADDR: {
        // EX: r = laddr(base + scale*index + disp)
        //
        // lea r, [..addr]

        X64_MemAddr addr;
        X64_get_lir_addr(builder, &addr, &ir_instr->laddr.addr);

        u32 r = X64_def_lir_reg(builder, ir_instr->laddr.r);
        X64_emit_instr_lea(builder, xbblock, r, addr);
        break;
    }
    case INSTR_STORE: {
        // EX: $addr = a
        //
        // mov [..addr], a

        size_t size = ir_instr->store.type->size;

        X64_MemAddr addr;
        X64_get_lir_addr(builder, &addr, &ir_instr->store.addr);

        u32 a = X64_get_lir_reg(builder, ir_instr->store.a);
        X64_emit_instr_mov_m_r(builder, xbblock, size, addr, a);
        break;
    }
    case INSTR_CMP: {
        size_t size = ir_instr->cmp.type->size;

        u32 a = X64_get_lir_reg(builder, ir_instr->cmp.a);
        u32 b = X64_get_lir_reg(builder, ir_instr->cmp.b);

        X64_emit_instr_cmp_r_r(builder, xbblock, size, a, b);

        bool combine_next = next_instr && (next_instr->kind == INSTR_COND_JMP) && (next_instr->cond_jmp.a == ir_instr->cmp.r);

        if (combine_next) {
            // Combine this comparison instruction with the next conditional jump.
            //
            // EX: r = a <cond> b
            //     cond_jmp r, <target>
            //
            //     BECOMES:
            //
            //     cmp a, b
            //     jmp_<cond> <target>
            
            X64_emit_instr_jmpcc(builder, xbblock, ir_instr->cmp.cond, NULL, NULL);
            ir_instr = next_instr;
        }
        else {
            // EX: r = a <cond> b
            //
            // cmp a, b
            // set_<cond> r

            u32 r = X64_def_lir_reg(builder, ir_instr->cmp.r);
            X64_emit_instr_setcc(builder, xbblock, ir_instr->cmp.cond, r);
        }

        break;
    }
    case INSTR_JMP: {
        X64_emit_instr_jmp(builder, xbblock, NULL);
        assert(!next_instr);
        break;
    }
    case INSTR_COND_JMP: {
        // EX: cond_jmp a, <target>
        //
        //     BECOMES
        //
        //     cmp a, 0
        //     jne <target>

        u32 a = X64_get_lir_reg(builder, ir_instr->cond_jmp.a);
        Scalar zero = {0};

        X64_emit_instr_cmp_r_i(builder, xbblock, 1, a, zero);
        X64_emit_instr_jmpcc(builder, xbblock, COND_NEQ, NULL, NULL);
        assert(!next_instr);
        break;
    }
    case INSTR_PHI: {
        // For now, just force all registers in PHI instruction into the same physical register.

        u32 r = X64_def_lir_reg(builder, ir_instr->phi.r);
        size_t num_args = ir_instr->phi.num_args;
        PhiArg* args = ir_instr->phi.args;

        for (size_t i = 0; i < num_args; i++) {
            u32 x = X64_get_lir_reg(builder, args[i].ireg);
            X64_alias_lir_regs(builder, x, r);
        }
        break;
    }
    case INSTR_MEMCPY: {
        // memcpy(dst, src, size)
        //
        // BECOMES
        //
        // lea rdi, [..dst]
        // lea rsi, [..src]
        // mov rcx, size
        // rep movsb
        X64_MemAddr dst_addr;
        X64_get_lir_addr(builder, &dst_addr, &ir_instr->memcpy.dst);

        u32 rdi = X64_get_lir_phys_reg(builder, X64_RDI);
        X64_emit_instr_lea(builder, xbblock, rdi, dst_addr);

        X64_MemAddr src_addr;
        X64_get_lir_addr(builder, &src_addr, &ir_instr->memcpy.src);

        u32 rsi = X64_get_lir_phys_reg(builder, X64_RSI);
        X64_emit_instr_lea(builder, xbblock, rsi, src_addr);

        Scalar num_bytes = {.as_int._u64 = ir_instr->memcpy.type->size};
        u32 rcx = X64_get_lir_phys_reg(builder, X64_RCX);
        X64_emit_instr_mov_r_i(builder, xbblock, PTR_SIZE, rcx, num_bytes);

        X64_emit_instr_rep_movsb(builder, xbblock);
        break;
    }
    case INSTR_RET: {
        // EX: ret a
        //
        //     BECOMES
        //
        //     mov _ax, a  ; if `a` not in `ax`
        //     ret
        Type* ret_type = ir_instr->ret.type;

        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            u32 a = X64_get_lir_reg(builder, ir_instr->ret.a);
            u32 ax = X64_get_lir_phys_reg(builder, X64_RAX);

            X64_emit_instr_mov_r_r(builder, xbblock, ret_type->size, ax, a);
        }

        X64_emit_instr_ret(builder, xbblock);
        break;
    }
    case INSTR_CALL_INDIRECT:
    case INSTR_CALL: {
        u32 num_args;
        InstrCallArg* args;
        Type* proc_type;
        IR_Reg ir_r;

        if (ir_instr->kind == INSTR_CALL) {
            num_args = ir_instr->call.num_args;
            args = ir_instr->call.args;
            proc_type = ir_instr->call.sym->type;
            ir_r = ir_instr->call.r;
        }
        else {
            num_args = ir_instr->calli.num_args;
            args = ir_instr->calli.args;
            proc_type = ir_instr->calli.proc_type;
            ir_r = ir_instr->calli.r;
        }

        X64_InstrCallArg* x64_args = alloc_array(builder->arena, X64_InstrCallArg, num_args, false);
        X64_StackArgsInfo stack_info = X64_convert_call_args(builder, xbblock, num_args, args, x64_args);

        Type* ret_type = proc_type->as_proc.ret;

        u32 r = (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) ? X64_def_lir_reg(builder, ir_r) : (u32)-1;

        if (ir_instr->kind == INSTR_CALL) {
            X64_emit_instr_call(builder, xbblock, ir_instr->call.sym, r, num_args, x64_args, stack_info);
        }
        else {
            u32 proc_r = X64_get_lir_reg(builder, ir_instr->calli.loc);
            X64_emit_instr_call_r(builder, xbblock, proc_type, proc_r, r, num_args, x64_args, stack_info);
        }

        break;
    }
    default:
        NIBBLE_FATAL_EXIT("[INTERNAL ERROR]: Unable to convert IR instruction %d to an X64 LIR instruction", ir_instr->kind);
        break;
    }

    return ir_instr->next;
}

static X64_BBlock* X64_make_bblock(X64_LIRBuilder* builder, BBlock* bblock)
{
    X64_BBlock* xbblock = alloc_type(builder->arena, X64_BBlock, true);
    xbblock->id = bblock->id;
    xbblock->flags = bblock->flags;
    xbblock->preds = array_create(builder->arena, X64_BBlock*, array_len(bblock->preds));

    array_push(builder->bblocks, xbblock);

    for (Instr* it = bblock->first; it;) {
        it = X64_convert_ir_instr(builder, xbblock, it);
    }

    return xbblock;
}

static X64_BBlock* X64_get_bblock_succ(X64_LIRBuilder* builder, Queue* queue, HMap* map, BBlock* n)
{
    X64_BBlock* xn = hmap_get_obj(map, PTR_UINT(n));

    // Haven't visited this neighbor yet.
    if (!xn) {
        xn = X64_make_bblock(builder, n);
        hmap_put(map, PTR_UINT(n), PTR_UINT(xn));

        queue_push(queue, n);
    }

    return xn;
}

static void X64_emit_lir_instrs(X64_LIRBuilder* builder, size_t num_bblocks, BBlock** bblocks)
{
    for (int i = 0; i < X64_REG_COUNT; i++) {
        builder->lreg_phys[i] = X64_next_lir_reg(builder);
    }

    // Clone graph using BFS and a map that maps old bblock to new bblock.
    Queue queue;
    queue_init(&queue, builder->arena);

    HMap map = hmap(clp2(num_bblocks), builder->arena);

    BBlock* start = bblocks[0];
    X64_BBlock* xstart = X64_make_bblock(builder, start);

    queue_push(&queue, start);
    hmap_put(&map, PTR_UINT(start), PTR_UINT(xstart));

    while (!queue_is_empty(&queue)) {
        BBlock* bb = queue_pop(&queue);

        X64_BBlock* xbb = hmap_get_obj(&map, PTR_UINT(bb));
        assert(xbb);

        Instr* instr = bb->last;
        X64_Instr* xinstr = xbb->last;

        if (instr->kind == INSTR_JMP) {
            assert(xinstr->kind == X64_INSTR_JMP);

            X64_BBlock* xn = X64_get_bblock_succ(builder, &queue, &map, instr->jmp.target);
            array_push(xn->preds, xbb);
            xinstr->jmp.target = xn;
        }
        else if (instr->kind == INSTR_COND_JMP) {
            assert(xinstr->kind == X64_INSTR_JMPCC);

            X64_BBlock* xn_true = X64_get_bblock_succ(builder, &queue, &map, instr->cond_jmp.true_bb);
            array_push(xn_true->preds, xbb);
            xinstr->jmpcc.true_bb = xn_true;

            X64_BBlock* xn_false = X64_get_bblock_succ(builder, &queue, &map, instr->cond_jmp.false_bb);
            array_push(xn_false->preds, xbb);
            xinstr->jmpcc.false_bb = xn_false;
        }
    }

    // Sort these basic blocks by ID.
    {
        X64_BBlock** xbblocks = builder->bblocks;
        size_t n = array_len(xbblocks);

        for (size_t i = 0; i < n; i++) {
            for (size_t j = 0; j < n - 1; j++) {
                X64_BBlock* curr = xbblocks[j];
                X64_BBlock* next = xbblocks[j + 1];

                if (curr->id > next->id) {
                    xbblocks[j] = next;
                    xbblocks[j + 1] = curr;
                }
            }
        }
    }

    // Renumber these instructions.
    {
        X64_BBlock** xbblocks = builder->bblocks;
        size_t n = array_len(xbblocks);
        long ino = 0;

        for (size_t i = 0; i < n; i++) {
            X64_BBlock* xbb = xbblocks[i];

            for (X64_Instr* it = xbb->first; it; it = it->next) {
                it->ino = ino;
                ino += 2;
            }
        }
    }
}

