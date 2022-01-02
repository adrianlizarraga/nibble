#include "ir.h"
#include "regs.h"

typedef struct X64_RegRange {
    u32 start;
    u32 end;
    X64_Reg reg;
} X64_RegRange;

typedef struct X64_InstrCallArg {
    Type* type;
    bool in_reg;
    
    union {
        u32 reg;
        u32 offset;
    };
} X64_InstrCallArg;

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
    range->end = array_len(builder->instrs) - 1;
}

static void X64_merge_ranges(X64_RegRange* dst_range, X64_RegRange* src_range)
{
    dst_range->start = dst_range->start <= src_range->start ? dst_range->start : src_range->start;
    dst_range->end = dst_range->end >= src_range->end ? dst_range->end : src_range->end;

    if (src_range->reg != X64_REG_COUNT) {
        assert(dst_range->reg == X64_REG_COUNT);
        dst_range->reg = src_range->reg;
    }
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

    range->end = next_ip;

    return result;
}

static u32 X64_def_phys_reg(X64_LLIRBuilder* builder, X64_Reg phys_reg)
{
    u32 result = X64_next_llir_reg(builder);
    X64_RegRange* range = &builder->llir_ranges[result];

    range->reg = phys_reg;
}

typedef enum X64_LLIRAddrKind {
    X64_LLIR_ADDR_GLOBAL,
    X64_LLIR_ADDR_LOCAL,
    X64_LLIR_ADDR_STR_LIT,
} X64_LLIRAddrKind;

typedef struct X64_LLIRAddr {
    X64_LLIRAddrKind kind;

    union {
        Symbol* global;
        struct {
            u32 base_reg;
            u32 index_reg;
            s32 disp;
            u8 scale;
        } local;
        StrLit* str_lit;
    };
} X64_LLIRAddr;

static void X64_get_llir_addr(X64_LLIRBuilder* builder, X64_LLIRAddr* dst, MemAddr* src)
{
    bool has_base = src->base_kind != MEM_BASE_NONE;
    bool has_index = src->scale && (src->index_reg < NIR_REG_COUNT);
    assert(has_base || has_index);

    if (has_base) {
        if (src->base_kind == MEM_BASE_STR_LIT) {
            dst->kind = X64_LLIR_ADDR_STR_LIT;
            dst->str_lit = src->base.str_lit;

            return;
        }

        if (src->base_kind == MEM_BASE_SYM) {
            Symbol* sym = src->base.sym;

            // Early exit for global variable addresses.
            if (!sym->is_local) {
                dst->kind = X64_LLIR_ADDR_GLOBAL;
                dst->global = sym;

                return;
            }

            dst->kind = X64_LLIR_ADDR_LOCAL;
            dst->local.base_reg = X64_def_phys_reg(builder, X64_RBP);
            dst->local.disp = src->disp + sym->as_var.offset;
            dst->local.scale = src->scale;
        }
        else {
            dst->kind = X64_LLIR_ADDR_LOCAL;
            dst->local.base_reg = X64_get_llir_reg(builder, src->base.reg);
            dst->local.disp = src->disp;
            dst->local.scale = src->scale;
        }

        if (has_index) {
            dst->local.index_reg = X64_get_llir_reg(builder, src->index_reg);
        }
        else {
            dst->local.index_reg = (u32)-1;
        }
    }
    else {
        dst->kind = X64_LLIR_ADDR_LOCAL;
        dst->local.base_reg = (u32)-1;
        dst->local.disp = src->disp;
        dst->local.scale = src->scale;
        dst->local.index_reg = X64_get_llir_reg(builder, src->index_reg);
    }
}

static X64_StackArgsInfo X64_linux_convert_call_args(X64_LLIRBuilder* builder, u32 num_args, InstrCallArg* args,
                                                     X64_InstrCallArg* x64_args)
{
    X64_StackArgsInfo stack_info = {0};
    u32 arg_reg_index = 0;

    for (u32 i = 0; i < num_args; i++) {
        InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;
        X64_InstrCallArg* arg_info = x64_args + i;

        assert(arg_size <= X64_MAX_INT_REG_SIZE); // TODO: Support structs

        if (arg_reg_index >= x64_target.num_arg_regs) {
            arg_info->in_reg = false;
            arg_info->offset = stack_info.args_size;

            stack_info.args_size += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }
        else {
            X64_Reg phys_reg = x64_target.arg_regs[arg_reg_index++];
            u32 a = X64_get_llir_reg(builder, arg->loc);
            X64_RegRange* a_rng = &builder->llir_ranges[a];

            if (a_rng->reg == X64_REG_COUNT || a_rng->reg == phys_reg) {
                a_rng->reg = phys_reg;
            }
            else {
               u32 dst_reg = X64_def_phys_reg(builder, phys_reg);

               X64_emit_mov_r_r(builder, arg_size, dst_reg, a);

               a = dst_reg;
            }

            arg_info->in_reg = true;
            arg_info->reg = a;
        }
    }

    return stack_info;
}

static X64_StackArgsInfo X64_windows_convert_call_args(X64_LLIRBuilder* builder, u32 num_args, InstrCallArg* args,
                                                       X64_InstrCallArg* x64_args)
{
    X64_StackArgsInfo stack_info = {.args_size = X64_WINDOWS_SHADOW_SPACE, .args_offset = X64_WINDOWS_SHADOW_SPACE};

    for (u32 i = 0; i < num_args; i++) {
        InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;
        X64_InstrCallArg* arg_info = x64_args + i;

        assert(arg_size <= X64_MAX_INT_REG_SIZE); // TODO: Support structs

        if (i >= x64_target.num_arg_regs) {
            arg_info->in_reg = false;
            arg_info->offset = stack_info.args_size;

            stack_info.args_size += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }
        else {
            X64_Reg phys_reg = x64_target.arg_regs[i];
            u32 a = X64_get_llir_reg(builder, arg->loc);
            X64_RegRange* a_rng = &builder->llir_ranges[a];

            if (a_rng->reg == X64_REG_COUNT || a_rng->reg == phys_reg) {
                a_rng->reg = phys_reg;
            }
            else {
               u32 dst_reg = X64_def_phys_reg(builder, phys_reg);

               X64_emit_mov_r_r(builder, arg_size, dst_reg, a);

               a = dst_reg;
            }

            arg_info->in_reg = true;
            arg_info->reg = a;
        }
    }

    return stack_info;
}

static X64_StackArgsInfo X64_convert_call_args(X64_LLIRBuilder* builder, u32 num_args, InstrCallArg* args, X64_InstrCallArg* x64_args)
{
    if (x64_target.os == OS_LINUX) {
        return X64_linux_convert_call_args(builder, num_args, args, x64_args);
    }
    else {
        return X64_windows_convert_call_args(builder, num_args, args, x64_args);
    }
}

static void X64_emit_llir_instrs(X64_LLIRBuilder* builder, size_t num_ir_instrs, Instr** ir_instrs)
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

    static X64_InstrKind convert_kind[] = {
        [INSTR_TRUNC] = X64_MOV,
        [INSTR_SEXT] = X64_MOVSX,
        [INSTR_ZEXT] = X64_MOVZX
    };

    size_t ip = 0;

    while (ip < num_ir_instrs) {
        Instr* ir_instr = ir_instrs[ip];
        bool is_last_instr = (ip == num_ir_instrs - 1);

        builder->next_instr_is_jmp_target = ir_instr->is_jmp_target;

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

            u32 a = X64_get_llir_reg(builder, ir_instr->binary.a);
            X64_RegRange* a_rng = &builder->llir_ranges[a];

            if (a_rng->reg == X64_RAX || a_rng->reg == X64_REG_COUNT) {
                a_rng->reg = X64_RAX;
            }
            else {
                u32 ax = X64_def_phys_reg(builder, X64_RAX);

                // mov _ax, a
                X64_emit_mov_r_r(builder, size, ax, a);

                a = ax; // Use ax as a
            }

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

            // mov r, a
            u32 r = X64_def_llir_reg(builder, ir_instr->binary.r);
            X64_emit_mov_r_r(builder, size, r, a);

            X64_end_reg_range(builder, a);
            break;
        }
        case INSTR_SAR:
        case INSTR_SHL: {
            size_t size = ir_instr->binary.type->size;

            // mov r, a
            u32 r = X64_def_llir_reg(builder, ir_instr->binary.r);
            u32 a = X64_get_llir_reg(builder, ir_instr->binary.a);
            X64_emit_mov_r_r(builder, size, r, a);

            u32 b = X64_get_llir_reg(builder, ir_instr->binary.b);
            X64_RegRange* b_rng = &builder->llir_ranges[b];

            if (b_rng->reg == X64_RCX || b_rng->reg == X64_REG_COUNT) {
                b_rng->reg = X64_RCX; // Force `cx`

                // shift r, b
                X64_emit_shift_r_r(builder, shift_kind[ir_instr->kind], size, r, b);
            }
            else {
                u32 cx = X64_def_phys_reg(builder, X64_RCX);

                // mov _cx, b
                // shift r, _cx
                X64_emit_mov_r_r(builder, size, cx, b);
                X64_emit_shift_r_r(builder, shift_kind[ir_instr->kind], size, r, cx);
                X64_end_reg_range(builder, cx);
            }

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
        case INSTR_TRUNC:
        case INSTR_ZEXT:
        case INSTR_SEXT: {
            // EX: r = sext(a)
            //
            // movsx r, a

            size_t dst_size = ir_instr->convert.dst_type->size;
            size_t src_size = ir_instr->convert.src_type->size;

            u32 r = X64_def_llir_reg(builder, ir_instr->convert.r);
            u32 a = X64_get_llir_reg(builder, ir_instr->convert.a);

            X64_emit_convert_r_r(builder, convert_kind[ir_instr->kind], dst_size, r, src_size, a);
            break;
        }
        case INSTR_LIMM: {
            // EX: r = 10
            //
            // mov r, 10

            size_t size = ir_instr->limm.type->size;

            u32 r = X64_def_llir_reg(builder, ir_instr->limm.r);
            
            X64_emit_limm(builder, size, r, ir_instr->limm.imm);
            break;
        }
        case INSTR_LOAD: {
            // EX: r = load(base + scale * index + disp)
            //
            // mov r, [base + scale * index + disp]

            size_t size = ir_instr->load.type->size;

            X64_LLIRAddr addr;
            X64_get_llir_addr(builder, &addr, &ir_instr->load.addr);

            u32 r = X64_def_llir_reg(builder, ir_instr->load.r);
            X64_emit_mov_r_m(builder, size, r, addr);
            break;
        }
        case INSTR_LADDR: {
            // EX: r = laddr(base + scale*index + disp)
            //
            // lea r, [..addr]

            X64_LLIRAddr addr;
            X64_get_llir_addr(builder, &addr, &ir_instr->laddr.addr);

            u32 r = X64_def_llir_reg(builder, ir_instr->laddr.r);
            X64_emit_lea(builder, r, addr);
            break;
        }
        case INSTR_STORE: {
            // EX: $addr = a
            //
            // mov [..addr], a

            size_t size = ir_instr->store.type->size;

            X64_LLIRAddr addr;
            X64_get_llir_addr(builder, &addr, &ir_instr->store.addr);

            u32 a = X64_def_llir_reg(builder, ir_instr->store.a);
            X64_emit_mov_m_r(builder, size, addr, a);
            break;
        }
        case INSTR_CMP: {
            size_t size = ir_instr->cmp.type->size;

            u32 a = X64_get_llir_reg(builder, ir_instr->cmp.a);
            u32 b = X64_get_llir_reg(builder, ir_instr->cmp.b);

            X64_emit_cmp_r_r(builder, size, a, b);

            bool combine_next = false;

            if (!is_last_instr) {
                Instr* next_instr = ir_instrs[ip + 1];

                combine_next = (next_instr->kind == INSTR_COND_JMP) && (next_instr->cond_jmp.a == ir_instr->cmp.r);
            }

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
                
                Instr* next_ir_instr = ir_instrs[++ip]; // NOTE: Increments `ip`
                // TODO: Jump target is wrong
                X64_emit_jmpcc(builder, ir_instr->cmp.cond, *next_ir_instr->cond_jmp.jmp_target);
            }
            else {
                // EX: r = a <cond> b
                //
                // cmp a, b
                // set_<cond> r

                u32 r = X64_def_llir_reg(builder, ir_instr->cmp.r);
                X64_emit_setcc(builder, ir_instr->cmp.cond, r);
            }

            break;
        }
        case INSTR_JMP: {
            // TODO: Jump target is wrong
            X64_emit_jmp(builder, *ir_instr->jmp.jmp_target);
            break;
        }
        case INSTR_COND_JMP: {
            // EX: cond_jmp a, <target>
            //
            //     BECOMES
            //
            //     cmp a, 0
            //     jne <target>

            u32 a = X64_get_llir_reg(builder, ir_instr->cond_jmp.a);
            Scalar zero = {0};

            X64_emit_cmp_r_i(builder, 1, a, zero);
            // TODO: Jump target is wrong
            X64_emit_jmpcc(builder, COND_NEQ, *ir_instr->cond_jmp.jmp_target);
            break;
        }
        case INSTR_PHI: {
            // For now, just force all registers in PHI instruction into the same physical register.

            u32 r = X64_get_llir_reg(builder, ir_instr->phi.r);
            u32 a = X64_get_llir_reg(builder, ir_instr->phi.a);
            u32 b = X64_get_llir_reg(builder, ir_instr->phi.b);

            X64_alias_llir_regs(builder, a, r);
            X64_alias_llir_regs(builder, b, r);

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
            X64_LLIRAddr dst_addr;
            X64_get_llir_addr(builder, &dst_addr, &ir_instr->memcpy.dst);

            u32 rdi = X64_def_phys_reg(builder, X64_RDI);
            X64_emit_lea(builder, rdi, dst_addr);

            X64_LLIRAddr src_addr;
            X64_get_llir_addr(builder, &src_addr, &ir_instr->memcpy.src);

            u32 rsi = X64_def_phys_reg(builder, X64_RSI);
            X64_emit_lea(builder, rsi, src_addr);

            Scalar num_bytes = {.as_int._u64 = ir_instr->memcpy.type->size};
            u32 rcx = X64_def_phys_reg(builder, X64_RCX);
            X64_emit_mov_r_i(builder, PTR_SIZE, rcx, num_bytes);

            X64_emit_rep_movsb(builder);

            X64_end_reg_range(builder, rdi);
            X64_end_reg_range(builder, rsi);
            X64_end_reg_range(builder, rcx);
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
                u32 a = X64_get_llir_reg(builder, ir_instr->ret.a);
                X64_RegRange* a_rng = &builder->llir_ranges[a];

                if (a_rng->reg == X64_RAX) {
                    X64_emit_ret(builder);
                }
                else if (a_rng->reg == X64_REG_COUNT) {
                    a_rng->reg = X64_RAX; // Force ax
                    X64_emit_ret(builder);
                }
                else {
                    u32 ax = X64_def_phys_reg(builder, X64_RAX);

                    X64_emit_mov_r_r(builder, ret_type->size, ax, a);
                    X64_emit_ret(builder);
                    X64_end_reg_range(builder, ax);
                }
            }
            else {
                X64_emit_ret(builder);
            }

            break;
        }
        case INSTR_CALL: {
            u32 num_args = ir_instr->call.num_args;
            InstrCallArgs* args = ir_instr->call.args;
            Type* proc_type = ir_instr->call.sym->type;
            NIR_Reg ir_r = ir_instr->call.r;

            X64_InstrCallArg* x64_args = alloc_array(builder->arena, X64_InstrCallArg, num_args, false);
            X64_StackArgsInfo stack_info = X64_convert_call_args(builder, num_args, args, x64_args);

            X64_emit_call(builder, ir_instr->call.sym, num_args, x64_args, stack_info);

            // Iterate through arg infos and call X64_end_reg_range() on each arg so that it lasts through the call
            for (u32 i = 0; i < num_args; i++) {
                X64_InstrCallArg* arg = x64_args + i;

                if (arg->in_reg) {
                    X64_end_reg_range(builder, arg->reg);
                }
            }

            Type* ret_type = proc_type->as_proc.ret;

            if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
                u32 r = X64_def_llir_reg(builder, ir_r);
                u32 ax = X64_def_phys_reg(builder, X64_RAX);

                X64_emit_mov_r_r(builder, ret_type->size, ax, a);
            }
            break;
        }
        default:
            NIBBLE_FATAL_EXIT("[INTERNAL ERROR]: Unable to convert IR instruction %d to an X64 LLIR instruction", ir_instr->kind);
            break;
        }

        ip += 1;
    }
}



