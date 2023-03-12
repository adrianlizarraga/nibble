#include "nibble.h"
#include "stream.h"
#include "x64_gen/module.h"
#include "x64_gen/regs.h"
#include "x64_gen/lir.h"
#include "x64_gen/reg_alloc.h"

typedef enum X64_SIBDAddrKind {
    X64_SIBD_ADDR_GLOBAL,
    X64_SIBD_ADDR_LOCAL,
    X64_SIBD_ADDR_STR_LIT,
    X64_SIBD_ADDR_FLOAT_LIT,
} X64_SIBDAddrKind;

typedef struct X64_SIBDAddr {
    X64_SIBDAddrKind kind;

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
} X64_SIBDAddr;

typedef enum X64InstrKind {
    X64InstrKind_NOOP = 0,
    X64InstrKind_PUSH,
    X64InstrKind_MOV_RR,
    X64InstrKind_MOV_MR,
    X64InstrKind_MOVSS_MR,
    X64InstrKind_MOVSD_MR,
    X64InstrKind_SUB_RI,
} X64InstrKind;

typedef struct X64Instr {
    X64InstrKind kind;

    union {
        struct {
            u8 reg;
        } push;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } mov_rr;

        struct {
            u8 size;
            X64_SIBDAddr dst;
            u8 src;
        } mov_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } sub_ri;

        struct {
            X64_SIBDAddr dst;
            u8 src;
        } movss_mr;

        struct {
            X64_SIBDAddr dst;
            u8 src;
        } movsd_mr;
    };
} X64Instr;

static void X64__emit_instr_push(Array(X64Instr) * instrs, X64_Reg reg)
{
    X64Instr push_instr = {
        .kind = X64InstrKind_PUSH,
        .push.reg = reg,
    };

    array_push(*instrs, push_instr);
}

static void X64__emit_instr_mov_rr(Array(X64Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64Instr mov_rr_instr = {
        .kind = X64InstrKind_MOV_RR,
        .mov_rr.size = size,
        .mov_rr.dst = dst,
        .mov_rr.src = src,
    };

    array_push(*instrs, mov_rr_instr);
}

static void X64__emit_instr_mov_mr(Array(X64Instr) * instrs, u8 size, X64_SIBDAddr dst, X64_Reg src)
{
    X64Instr mov_mr_instr = {
        .kind = X64InstrKind_MOV_MR,
        .mov_mr.size = size,
        .mov_mr.dst = dst,
        .mov_mr.src = src,
    };

    array_push(*instrs, mov_mr_instr);
}

static void X64__emit_instr_movss_mr(Array(X64Instr) * instrs, X64_SIBDAddr dst, X64_Reg src)
{
    X64Instr movss_mr_instr = {
        .kind = X64InstrKind_MOVSS_MR,
        .movss_mr.dst = dst,
        .movss_mr.src = src,
    };

    array_push(*instrs, movss_mr_instr);
}

static void X64__emit_instr_movsd_mr(Array(X64Instr) * instrs, X64_SIBDAddr dst, X64_Reg src)
{
    X64Instr movsd_mr_instr = {
        .kind = X64InstrKind_MOVSD_MR,
        .movsd_mr.dst = dst,
        .movsd_mr.src = src,
    };

    array_push(*instrs, movsd_mr_instr);
}

static size_t X64__emit_instr_placeholder(Array(X64Instr) * instrs, X64InstrKind kind)
{
    X64Instr instr = {.kind = kind};
    array_push(*instrs, instr);
    return array_len(*instrs) - 1;
}

typedef struct X64ProcState {
    Allocator* gen_mem;
    Allocator* tmp_mem;
    Symbol* sym; // Procedure symbol.
    u32 id; // Procedure ID.
    const X64_LIRBuilder* builder;
    X64_ScratchRegs (*scratch_regs)[X64_REG_CLASS_COUNT];
    Array(X64Instr) instrs;
} X64ProcState;

typedef struct X64_StackParamsInfo {
    u64 stack_spill_size; // Spill size below rsp
    List* local_var_iter; // Iterator pointing to the first local variable (if any) of the proc
} X64_StackParamsInfo;

typedef struct X64_LinuxAssignParamState {
    u64 stack_spill_size;
    u64 stack_arg_offset;
} X64_LinuxAssignParamState;

static s32 X64_consume_stack_arg(u64* stack_arg_offset, u64 arg_size, u64 arg_align)
{
    s32 offset = (s32)*stack_arg_offset;

    *stack_arg_offset += arg_size;
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, arg_align);
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, X64_STACK_WORD_SIZE);

    return offset;
}

static s32 X64_spill_reg(Array(X64Instr) * instrs, X64_LinuxAssignParamState* state, u64 size, u64 align, X64_Reg preg)
{
    state->stack_spill_size += size;
    state->stack_spill_size = ALIGN_UP(state->stack_spill_size, align);
    s32 offset = -state->stack_spill_size;

    X64_RegClass reg_class = x64_reg_classes[preg];
    X64_SIBDAddr dst_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.index_reg = -1, .local.disp = offset};

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

static void X64_assign_proc_param_offsets(Array(X64Instr) * instrs, const Symbol* sproc, X64_StackParamsInfo* stack_params_info)
{
    const DeclProc* dproc = (const DeclProc*)sproc->decl;
    const Type* ret_type = sproc->type->as_proc.ret;

    u32 index = 0;
    u32 arg_reg_indices[X64_REG_CLASS_COUNT] = {0};
    X64_LinuxAssignParamState state = {.stack_arg_offset = 0x10};

    // For procs that return a large struct by value:
    // Spill the first argument, which contains a pointer to the return value's memory address, into the stack.
    // We need to spill (remember) this address so that the procedure can return it, as per the X64 calling conventions.
    if (type_is_obj_like(ret_type) && X64_is_obj_retarg_large(ret_type->size)) {
        X64_ScratchRegs arg_int_regs = (*x64_target.arg_regs)[X64_REG_CLASS_INT];

        X64_spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, X64_MAX_INT_REG_SIZE,
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

                sym->as_var.offset = X64_spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, arg_reg);
            }
            else if ((arg_size <= (X64_MAX_INT_REG_SIZE << 1)) && (rem_regs >= 2)) {
                X64_Reg low_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;
                X64_Reg high_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                X64_spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, high_reg);
                sym->as_var.offset = X64_spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, low_reg);
            }
            else {
                sym->as_var.offset = X64_consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
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

                sym->as_var.offset = X64_spill_reg(instrs, &state, arg_size, arg_align, arg_reg);
            }
            else {
                sym->as_var.offset = X64_consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = state.stack_spill_size;
    stack_params_info->local_var_iter = it;
}

static u64 X64_assign_scope_stack_offsets(Scope* scope, u64 offset)
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
            u64 child_size = X64_assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static u64 X64_assign_proc_stack_offsets(X64ProcState* proc_state)
{
    Symbol* sproc = proc_state->sym;
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Scope* scope = dproc->scope;

    //
    // Spill procedure params into the stack (assign stack offsets to params).
    //

    X64_StackParamsInfo stack_params_info = {0};
    X64_assign_proc_param_offsets(&proc_state->instrs, sproc, &stack_params_info);

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
            u32 child_size = X64_assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

Array(X64Instr) X64_gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym, u32 proc_id)
{
    const bool is_nonleaf = proc_sym->as_proc.is_nonleaf;

    X64ProcState state = {
        .gen_mem = gen_mem,
        .tmp_mem = tmp_mem,
        .sym = proc_sym,
        .id = proc_id,
        .instrs = array_create(gen_mem, X64Instr, 64),
        .scratch_regs = is_nonleaf ? x64_target.nonleaf_scratch_regs : x64_target.leaf_scratch_regs,
    };

    AllocatorState tmp_mem_state = allocator_get_state(state.tmp_mem);
    //////////////////////////////////////////////////////////////////////////////////////////

    X64__emit_instr_push(&state.instrs, X64_RBP);
    X64__emit_instr_mov_rr(&state.instrs, X64_MAX_INT_REG_SIZE, X64_RBP, X64_RSP);

    // Add a placeholder sub rsp, <stack_size> instruction. Will fill in once we know the procedure's stack size.
    const size_t sub_rsp_idx = X64__emit_instr_placeholder(&state.instrs, X64InstrKind_SUB_RI);

    u32 stack_size = X64_assign_proc_stack_offsets(&state);

    //////////////////////////////////////////////////////////////////////////////////////////
    allocator_restore_state(tmp_mem_state);

    return state.instrs;
}
