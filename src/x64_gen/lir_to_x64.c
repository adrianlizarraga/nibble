#include "allocator.h"
#include "array.h"
#include "hash_map.h"
#include "nibble.h"
#include "stream.h"
#include "x64_gen/module.h"
#include "x64_gen/regs.h"
#include "x64_gen/lir.h"
#include "x64_gen/reg_alloc.h"

typedef enum X64_SIBD_Addr_Kind {
    X64_SIBD_ADDR_GLOBAL,
    X64_SIBD_ADDR_LOCAL,
    X64_SIBD_ADDR_STR_LIT,
    X64_SIBD_ADDR_FLOAT_LIT,
} X64_SIBD_Addr_Kind;

typedef struct X64_SIBD_Addr {
    X64_SIBD_Addr_Kind kind;

    union {
        Symbol* global;
        struct {
            u8 base_reg;
            u8 index_reg;
            u8 scale;
            s32 disp;
        } local;
        StrLit* str_lit;
        FloatLit* float_lit;
    };
} X64_SIBD_Addr;

X64_SIBD_Addr X64__get_stack_offset_addr(s32 offset)
{
    X64_SIBD_Addr addr = {
        .kind = X64_SIBD_ADDR_LOCAL,
        .local = {
            .base_reg = X64_RBP,
            .index_reg = -1,
            .disp = offset
        }
    };

    return addr;
}

typedef enum X64_Instr_Kind {
    X64_Instr_Kind_NOOP = 0,
    X64_Instr_Kind_RET,
    X64_Instr_Kind_JMP,
    X64_Instr_Kind_JMP_TO_RET, // Doesn't correspond to an actual X64 instruction. Jumps to ret label.
    X64_Instr_Kind_PUSH,
    X64_Instr_Kind_POP,
    X64_Instr_Kind_ADD_RR,
    X64_Instr_Kind_ADD_RM,
    X64_Instr_Kind_ADD_MR,
    X64_Instr_Kind_ADD_RI,
    X64_Instr_Kind_SUB_RI,
    X64_Instr_Kind_MOV_RR,
    X64_Instr_Kind_MOV_MR,
    X64_Instr_Kind_MOVSS_MR,
    X64_Instr_Kind_MOVSD_MR,
    X64_Instr_Kind_MOVDQU_MR,
    X64_Instr_Kind_MOVDQU_RM,

    X64_Instr_Kind_COUNT
} X64_Instr_Kind;

typedef struct X64__Instr {
    X64_Instr_Kind kind;

    union {
        struct {
            u8 reg;
        } push;

        struct {
            u8 reg;
        } pop;

        struct {
            size_t target;
        } jmp; // Also for JMP_TO_RET

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } add_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } add_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } add_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } add_ri;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } sub_ri;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } mov_rr;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } mov_mr;

        struct {
            X64_SIBD_Addr dst;
            u8 src;
        } movss_mr;

        struct {
            X64_SIBD_Addr dst;
            u8 src;
        } movsd_mr;

        struct {
            X64_SIBD_Addr dst;
            u8 src;
        } movdqu_mr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } movdqu_rm;
    };
} X64__Instr;

static void X64__emit_instr_ret(Array(X64__Instr) * instrs)
{
    X64__Instr ret_instr = {.kind = X64_Instr_Kind_RET};
    array_push(*instrs, ret_instr);
}

static void X64__emit_instr_push(Array(X64__Instr) * instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64__Instr push_instr = {
        .kind = X64_Instr_Kind_PUSH,
        .push.reg = reg,
    };

    array_push(*instrs, push_instr);
}

static void X64__emit_instr_pop(Array(X64__Instr) * instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64__Instr pop_instr = {
        .kind = X64_Instr_Kind_POP,
        .pop.reg = reg,
    };

    array_push(*instrs, pop_instr);
}

static void X64__emit_instr_add_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr add_rr_instr = {
        .kind = X64_Instr_Kind_ADD_RR,
        .add_rr.size = size,
        .add_rr.dst = dst,
        .add_rr.src = src,
    };

    array_push(*instrs, add_rr_instr);
}

static void X64__emit_instr_add_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr add_rm_instr = {
        .kind = X64_Instr_Kind_ADD_RM,
        .add_rm.size = size,
        .add_rm.dst = dst,
        .add_rm.src = src,
    };

    array_push(*instrs, add_rm_instr);
}

static void X64__emit_instr_add_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr add_mr_instr = {
        .kind = X64_Instr_Kind_ADD_MR,
        .add_mr.size = size,
        .add_mr.dst = dst,
        .add_mr.src = src,
    };

    array_push(*instrs, add_mr_instr);
}

static void X64__emit_instr_add_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr add_ri_instr = {
        .kind = X64_Instr_Kind_ADD_RI,
        .add_ri.size = size,
        .add_ri.dst = dst,
        .add_ri.imm = imm,
    };

    array_push(*instrs, add_ri_instr);
}

static void X64__emit_instr_sub_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr sub_ri_instr = {
        .kind = X64_Instr_Kind_SUB_RI,
        .sub_ri.size = size,
        .sub_ri.dst = dst,
        .sub_ri.imm = imm,
    };

    array_push(*instrs, sub_ri_instr);
}

static void X64__emit_instr_mov_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr mov_rr_instr = {
        .kind = X64_Instr_Kind_MOV_RR,
        .mov_rr.size = size,
        .mov_rr.dst = dst,
        .mov_rr.src = src,
    };

    array_push(*instrs, mov_rr_instr);
}

static void X64__emit_instr_mov_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr mov_mr_instr = {
        .kind = X64_Instr_Kind_MOV_MR,
        .mov_mr.size = size,
        .mov_mr.dst = dst,
        .mov_mr.src = src,
    };

    array_push(*instrs, mov_mr_instr);
}

static void X64__emit_instr_movss_mr(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr movss_mr_instr = {
        .kind = X64_Instr_Kind_MOVSS_MR,
        .movss_mr.dst = dst,
        .movss_mr.src = src,
    };

    array_push(*instrs, movss_mr_instr);
}

static void X64__emit_instr_movsd_mr(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr movsd_mr_instr = {
        .kind = X64_Instr_Kind_MOVSD_MR,
        .movsd_mr.dst = dst,
        .movsd_mr.src = src,
    };

    array_push(*instrs, movsd_mr_instr);
}

static void X64__emit_instr_movdqu_mr(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    assert(x64_reg_classes[src] == X64_REG_CLASS_FLOAT);
    X64__Instr movdqu_mr_instr = {
        .kind = X64_Instr_Kind_MOVDQU_MR,
        .movdqu_mr.dst = dst,
        .movdqu_mr.src = src,
    };

    array_push(*instrs, movdqu_mr_instr);
}

static void X64__emit_instr_movdqu_rm(Array(X64__Instr) * instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    assert(x64_reg_classes[dst] == X64_REG_CLASS_FLOAT);
    X64__Instr movdqu_rm_instr = {
        .kind = X64_Instr_Kind_MOVDQU_RM,
        .movdqu_rm.dst = dst,
        .movdqu_rm.src = src,
    };

    array_push(*instrs, movdqu_rm_instr);
}

static size_t X64__emit_instr_placeholder(Array(X64__Instr) * instrs, X64_Instr_Kind kind)
{
    X64__Instr instr = {.kind = kind};
    array_push(*instrs, instr);
    return array_len(*instrs) - 1;
}

static void X64__push_reg_to_stack(Array(X64__Instr)* instrs, X64_Reg reg)
{
    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        X64__emit_instr_push(instrs, reg);
    }
    else {
        X64__emit_instr_sub_ri(instrs, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Make room for 16 bytes on the stack.

        X64_SIBD_Addr dst = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP};
        X64__emit_instr_movdqu_mr(instrs, dst, reg); // movdqu oword [rsp], reg
    }
}

static void X64__pop_reg_from_stack(Array(X64__Instr)* instrs, X64_Reg reg)
{
    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        X64__emit_instr_pop(instrs, reg);
    }
    else {
        X64_SIBD_Addr src = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP};
        X64__emit_instr_movdqu_rm(instrs, reg, src); // movdqu reg, oword [rsp]

        X64__emit_instr_add_ri(instrs, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Clean up 16 bytes from stack.
    }
}

typedef void X64_Emit_Bin_Op_RR_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_Reg src);
typedef void X64_Emit_Bin_Op_RM_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
typedef void X64_Emit_Bin_Op_MR_Func (Array(X64__Instr)* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);

static X64_Emit_Bin_Op_RR_Func* x64_bin_op_rr_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_ADD_RR] = X64__emit_instr_add_rr,
};

static X64_Emit_Bin_Op_RM_Func* x64_bin_op_rm_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_ADD_RM] = X64__emit_instr_add_rm,
};

static X64_Emit_Bin_Op_MR_Func* x64_bin_op_mr_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_ADD_MR] = X64__emit_instr_add_mr,
};

typedef struct X64_Proc_State {
    Allocator* gen_mem;
    Allocator* tmp_mem;
    Symbol* sym; // Procedure symbol.
    u32 id; // Procedure ID.
    X64_LIRBuilder* builder;
    X64_ScratchRegs (*scratch_regs)[X64_REG_CLASS_COUNT];
    Array(X64__Instr) instrs;
} X64_Proc_State;

static X64_LRegLoc X64__lreg_loc(X64_Proc_State* proc_state, u32 lreg)
{
    u32 rng_idx = X64_find_alias_reg(proc_state->builder, lreg);
    X64_LRegLoc reg_loc = proc_state->builder->lreg_ranges[rng_idx].loc;

    assert(reg_loc.kind != X64_LREG_LOC_UNASSIGNED);

    return reg_loc;
}

static void X64__emit_bin_op_rr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, X64_RegClass reg_class,
                                      u32 op_size, u32 op1_lreg, u32 op2_lreg)
{
    X64_LRegLoc op1_loc = X64__lreg_loc(proc_state, op1_lreg);
    X64_LRegLoc op2_loc = X64__lreg_loc(proc_state, op2_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            x64_bin_op_rr_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_loc.reg, op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            x64_bin_op_rm_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_loc.reg, X64__get_stack_offset_addr(op2_loc.offset));
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    case X64_LREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            x64_bin_op_mr_emit_funcs[instr_kind](&proc_state->instrs, op_size, X64__get_stack_offset_addr(op1_loc.offset), op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            const X64_SIBD_Addr op1_addr = X64__get_stack_offset_addr(op1_loc.offset);
            const X64_SIBD_Addr op2_addr = X64__get_stack_offset_addr(op2_loc.offset);

            X64_Reg tmp_reg = (reg_class == X64_REG_CLASS_INT) ? X64_RAX : X64_XMM0;
            //const char* tmp_reg_str = X64_reg_name(tmp_reg, op1_size);

            /*
            // Save the contents of a temporary register into the stack.
            X64_print_push_reg(generator, generator->curr_proc.text_lines.prev, tmp_reg);

            // Load dst into the temporary register,
            const char* mov_instr =
                (reg_class == X64_REG_CLASS_FLOAT) ? (op1_size == float_kind_sizes[FLOAT_F64] ? "movsd" : "movss") : "mov";
            X64_emit_text(generator, "  %s %s, %s", mov_instr, tmp_reg_str, op1_op_str);

            // Execute the instruction using the temporary register as the destination.
            X64_emit_text(generator, "  %s %s, %s", instr, tmp_reg_str, op2_op_str);

            // Store the result of the instruction (contents of temporary register) into dst.
            if (writes_op1) {
                X64_emit_text(generator, "  %s %s, %s", mov_instr, op1_op_str, tmp_reg_str);
            }

            // Restore the contents of the temporary register.
            X64_print_pop_reg(generator, generator->curr_proc.text_lines.prev, tmp_reg);
            */

            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64__gen_instr(X64_Proc_State* proc_state, X64_Instr* instr, bool is_last_instr, long bblock_id)
{
    AllocatorState mem_state = allocator_get_state(proc_state->tmp_mem);

    allocator_restore_state(mem_state);
}

typedef struct X64_Stack_Params_Info {
    u64 stack_spill_size; // Spill size below rsp
    List* local_var_iter; // Iterator pointing to the first local variable (if any) of the proc
} X64_Stack_Params_Info;

typedef struct X64_Stack_Spill_State {
    u64 stack_spill_size;
    u64 stack_arg_offset;
} X64_Stack_Spill_State;

static s32 X64__consume_stack_arg(u64* stack_arg_offset, u64 arg_size, u64 arg_align)
{
    s32 offset = (s32)*stack_arg_offset;

    *stack_arg_offset += arg_size;
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, arg_align);
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, X64_STACK_WORD_SIZE);

    return offset;
}

static s32 X64__spill_reg(Array(X64__Instr) * instrs, X64_Stack_Spill_State* state, u64 size, u64 align, X64_Reg preg)
{
    state->stack_spill_size += size;
    state->stack_spill_size = ALIGN_UP(state->stack_spill_size, align);
    s32 offset = -state->stack_spill_size;

    X64_RegClass reg_class = x64_reg_classes[preg];
    X64_SIBD_Addr dst_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.index_reg = -1, .local.disp = offset};

    if (reg_class == X64_REG_CLASS_INT) {
        X64__emit_instr_mov_mr(instrs, (u8)size, dst_addr, preg);
        return offset;
    }

    assert(reg_class == X64_REG_CLASS_FLOAT);
    if (size == float_kind_sizes[FLOAT_F64]) {
        X64__emit_instr_movsd_mr(instrs, dst_addr, preg);
    }
    else {
        X64__emit_instr_movss_mr(instrs, dst_addr, preg);
    }

    return offset;
}

static void X64__assign_proc_param_offsets(Array(X64__Instr) * instrs, const Symbol* sproc, X64_Stack_Params_Info* stack_params_info)
{
    const DeclProc* dproc = (const DeclProc*)sproc->decl;
    const Type* ret_type = sproc->type->as_proc.ret;

    u32 index = 0;
    u32 arg_reg_indices[X64_REG_CLASS_COUNT] = {0};
    X64_Stack_Spill_State state = {.stack_arg_offset = 0x10};

    // For procs that return a large struct by value:
    // Spill the first argument, which contains a pointer to the return value's memory address, into the stack.
    // We need to spill (remember) this address so that the procedure can return it, as per the X64 calling conventions.
    if (type_is_obj_like(ret_type) && X64_is_obj_retarg_large(ret_type->size)) {
        X64_ScratchRegs arg_int_regs = (*x64_target.arg_regs)[X64_REG_CLASS_INT];

        X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, X64_MAX_INT_REG_SIZE,
                       arg_int_regs.regs[arg_reg_indices[X64_REG_CLASS_INT]]);
        arg_reg_indices[X64_REG_CLASS_INT] += 1;
    }

    Scope* scope = dproc->scope;
    List* head = &scope->sym_list;
    List* it = head->next;

    while (it != head) {
        // Only process params. Local variables are not processed here.
        if (index >= dproc->num_params)
            break;

        Symbol* sym = list_entry(it, Symbol, lnode);

        // Assign stack offsets to procedure params.
        assert(sym->kind == SYMBOL_VAR);

        Type* arg_type = sym->type;
        u64 arg_size = arg_type->size;
        u64 arg_align = arg_type->align;

        if (type_is_obj_like(arg_type)) {
            X64_RegClass reg_class = X64_obj_reg_class(arg_type);
            X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];
            u32* arg_reg_index = &arg_reg_indices[reg_class];

            u32 rem_regs = arg_regs.num_regs - *arg_reg_index;

            if ((arg_size <= X64_MAX_INT_REG_SIZE) && (rem_regs >= 1)) {
                X64_Reg arg_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                sym->as_var.offset = X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, arg_reg);
            }
            else if ((arg_size <= (X64_MAX_INT_REG_SIZE << 1)) && (rem_regs >= 2)) {
                X64_Reg low_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;
                X64_Reg high_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, high_reg);
                sym->as_var.offset = X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, low_reg);
            }
            else {
                sym->as_var.offset = X64__consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }
        else {
            X64_RegClass reg_class = arg_type->kind == TYPE_FLOAT ? X64_REG_CLASS_FLOAT : X64_REG_CLASS_INT;
            X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];
            u32* arg_reg_index = &arg_reg_indices[reg_class];

            // Spill argument register below rsp
            if (*arg_reg_index < arg_regs.num_regs) {
                X64_Reg arg_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                sym->as_var.offset = X64__spill_reg(instrs, &state, arg_size, arg_align, arg_reg);
            }
            else {
                sym->as_var.offset = X64__consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = state.stack_spill_size;
    stack_params_info->local_var_iter = it;
}

static u64 X64__assign_scope_stack_offsets(Scope* scope, u64 offset)
{
    u64 stack_size = offset;

    //
    // Sum sizes of local variables declared in this scope.
    //
    {
        List* head = &scope->sym_list;
        List* it = head->next;

        while (it != head) {
            Symbol* sym = list_entry(it, Symbol, lnode);

            if (sym->kind == SYMBOL_VAR) {
                stack_size += sym->type->size;
                stack_size = ALIGN_UP(stack_size, sym->type->align);
                sym->as_var.offset = -stack_size;
            }

            it = it->next;
        }
    }

    //
    // Sum sizes of anonymous objects in this scope.
    //
    {
        List* head = &scope->obj_list;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //
    {
        List* head = &scope->children;
        List* it = head->next;
        u64 child_offset = stack_size;

        while (it != head) {
            Scope* child_scope = list_entry(it, Scope, lnode);
            u64 child_size = X64__assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static u64 X64__assign_proc_stack_offsets(X64_Proc_State* proc_state)
{
    Symbol* sproc = proc_state->sym;
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Scope* scope = dproc->scope;

    //
    // Spill procedure params into the stack (assign stack offsets to params).
    //

    X64_Stack_Params_Info stack_params_info = {0};
    X64__assign_proc_param_offsets(&proc_state->instrs, sproc, &stack_params_info);

    u64 stack_size = stack_params_info.stack_spill_size;

    //
    // Assign stack offsets to local variables declared in the procedure's top scope.
    //

    {
        List* it = stack_params_info.local_var_iter;
        List* head = &scope->sym_list;

        while (it != head) {
            Symbol* sym = list_entry(it, Symbol, lnode);

            // Assign stack offsets to local variables in procedure.
            if (sym->kind == SYMBOL_VAR) {
                stack_size += sym->type->size;
                stack_size = ALIGN_UP(stack_size, sym->type->align);
                sym->as_var.offset = -stack_size;
            }

            it = it->next;
        }
    }

    //
    // Sum sizes of `TEMPORARY` anonymous objects in the procedure's top scope.
    //
    {
        List* head = &sproc->as_proc.tmp_objs;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

            it = it->next;
        }
    }

    //
    // Sum sizes of anonymous objects in the procedure's top scope.
    //
    {
        List* head = &scope->obj_list;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //

    {
        List* head = &scope->children;
        List* it = head->next;
        u64 child_offset = stack_size;

        while (it != head) {
            Scope* child_scope = list_entry(it, Scope, lnode);
            u32 child_size = X64__assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static void X64__patch_jmp_instrs(X64__Instr* instrs, size_t num_instrs, const HMap* bblock_instr_starts)
{
    for (size_t i = 0; i < num_instrs; ++i) {
        X64__Instr* instr = &instrs[i];

        if (instr->kind == X64_Instr_Kind_JMP) {
            // jmp.target initially contains the target bblock ID.
            // Use the map to get the bblock's starting instruction index.
            size_t* instr_index = hmap_get(bblock_instr_starts, instr->jmp.target);
            assert(instr_index != NULL);
            assert(*instr_index < num_instrs);
            instr->jmp.target = *instr_index;
        }
        else if (instr->kind == X64_Instr_Kind_JMP_TO_RET) {
            // Jump after the last instruction (postamble).
            instr->jmp.target = num_instrs;
        }
    }
}

Array(X64__Instr) X64__gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym, u32 proc_id)
{
    const bool is_nonleaf = proc_sym->as_proc.is_nonleaf;

    X64_Proc_State state = {
        .gen_mem = gen_mem,
        .tmp_mem = tmp_mem,
        .sym = proc_sym,
        .id = proc_id,
        .instrs = array_create(gen_mem, X64__Instr, 64),
        .scratch_regs = is_nonleaf ? x64_target.nonleaf_scratch_regs : x64_target.leaf_scratch_regs,
    };

    AllocatorState tmp_mem_state = allocator_get_state(state.tmp_mem);
    //////////////////////////////////////////////////////////////////////////////////////////

    X64__emit_instr_push(&state.instrs, X64_RBP);
    X64__emit_instr_mov_rr(&state.instrs, X64_MAX_INT_REG_SIZE, X64_RBP, X64_RSP);
    const size_t sub_rsp_idx = X64__emit_instr_placeholder(&state.instrs, X64_Instr_Kind_NOOP);  // Placeholder sub rsp, <stack_size>

    // Calculate stack size from procedure arguments (spills) and local variables.
    u32 stack_size = X64__assign_proc_stack_offsets(&state);

    // Register allocation.
    BBlock** ir_bblocks = proc_sym->as_proc.bblocks;
    size_t num_ir_bblocks = array_len(ir_bblocks);
    X64_LIRBuilder builder = {.arena = tmp_mem};

    X64_emit_lir_instrs(&builder, proc_sym->as_proc.num_regs, num_ir_bblocks, ir_bblocks); // Generate LIR instructions.
    X64_compute_live_intervals(&builder); // Compute LIR register intervals.
    X64_RegAllocResult reg_alloc = X64_linear_scan_reg_alloc(&builder, state.scratch_regs, stack_size);  // May spill and increase stack_size.

    if (!reg_alloc.success) {
        NIBBLE_FATAL_EXIT("Register allocation for procedure `%s` failed.", proc_sym->name->str);
        return NULL;
    }

    stack_size = reg_alloc.stack_offset;
    state.builder = &builder;

    // Fill in sub rsp, <stack_size>
    if (stack_size) {
        X64__Instr* sub_rsp_instr = &state.instrs[sub_rsp_idx];
        sub_rsp_instr->kind = X64_Instr_Kind_SUB_RI;
        sub_rsp_instr->sub_ri.size = X64_MAX_INT_REG_SIZE;
        sub_rsp_instr->sub_ri.dst = X64_RSP;
        sub_rsp_instr->sub_ri.imm = stack_size;
    }

    // Emit instructions to save callee-saved registers.
    // NOTE: The procedure instructions may need to dynamically access callee-saved registers,
    // but they will always be saved and restored.
    for (uint32_t r = 0; r < X64_REG_COUNT; r += 1) {
        X64_Reg reg = (X64_Reg)r;

        if (reg == X64_RBP || reg == X64_RSP)
            continue;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            X64__push_reg_to_stack(&state.instrs, reg);
        }
    }

    // Process procedure instructions.
    HMap bblock_instr_starts = hmap(clp2(num_ir_bblocks), tmp_mem);

    for (size_t ii = 0; ii < builder.num_bblocks; ii++) {
        X64_BBlock* bb = builder.bblocks[ii];
        bool last_bb = ii == builder.num_bblocks - 1;

        hmap_put(&bblock_instr_starts, bb->id, array_len(state.instrs));

        for (X64_Instr* instr = bb->first; instr; instr = instr->next) {
            bool last_instr = last_bb && !instr->next;

            X64__gen_instr(&state, instr, last_instr, bb->id);
        }
    }

    // Patch jmp (to block, to ret label) instructions.
    X64__patch_jmp_instrs(state.instrs, array_len(state.instrs), &bblock_instr_starts);

    // Restore callee-saved registers.
    // NOTE: Iterating in the reverse order as the corresponding pushes.
    for (uint32_t r = X64_REG_COUNT; r-- > 0;) {
        X64_Reg reg = (X64_Reg)r;

        if (reg == X64_RBP || reg == X64_RSP)
            continue;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            X64__pop_reg_from_stack(&state.instrs, reg);
        }
    }

    // Postamble.
    X64__emit_instr_mov_rr(&state.instrs, X64_MAX_INT_REG_SIZE, X64_RSP, X64_RBP);
    X64__emit_instr_pop(&state.instrs, X64_RBP);
    X64__emit_instr_ret(&state.instrs);

    //////////////////////////////////////////////////////////////////////////////////////////
    allocator_restore_state(tmp_mem_state);

    return state.instrs;
}
