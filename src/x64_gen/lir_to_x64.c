#include "stream.h"
#include "x64_gen/module.h"
#include "x64_gen/regs.h"
#include "x64_gen/lir.h"
#include "x64_gen/reg_alloc.h"

typedef enum X64InstrKind {
    X64InstrKind_NOOP = 0,
    X64InstrKind_PUSH,
    X64InstrKind_MOV_RR,
    X64InstrKind_SUB_RI,
} X64InstrKind;

typedef struct X64Instr {
    X64InstrKind kind;

    union {
        struct {
            u8 reg;
	} push;

        struct {
            u8 dst;
            u8 src;
	} mov_rr;

        struct {
            u8 dst;
            u32 imm;
	} sub_ri;
    };
} X64Instr;

static void X64__emit_instr_push(Array(X64Instr)* instrs, X64_Reg reg)
{
    X64Instr push_instr = {
        .kind = X64InstrKind_PUSH,
	.push.reg = reg,
    };

    array_push(*instrs, push_instr);
}

static void X64__emit_instr_mov_rr(Array(X64Instr)* instrs, X64_Reg dst, X64_Reg src)
{
    X64Instr mov_rr_instr = {
        .kind = X64InstrKind_MOV_RR,
	.mov_rr.dst = dst,
	.mov_rr.src = src,
    };

    array_push(*instrs, mov_rr_instr);
}

static size_t X64__emit_instr_placeholder(Array(X64Instr)* instrs, X64InstrKind kind)
{
    X64Instr instr = { .kind = kind };
    array_push(*instrs, instr);
    return array_len(*instrs) - 1;
}

typedef struct X64ProcState {
    Allocator* gen_mem;
    Allocator* tmp_mem;
    const Symbol* sym; // Procedure symbol.
    u32 id; // Procedure ID.
    const X64_LIRBuilder* builder;
    X64_ScratchRegs (*scratch_regs)[X64_REG_CLASS_COUNT];
    Array(X64Instr) instrs;
} X64ProcState;

Array(X64Instr) X64_gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, const Symbol* proc_sym, u32 proc_id)
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
    X64__emit_instr_mov_rr(&state.instrs, X64_RBP, X64_RSP);

    // Add a placeholder sub rsp, <stack_size> instruction. Will fill in once we know the procedure's stack size.
    const size_t sub_rsp_idx = X64__emit_instr_placeholder(&state.instrs, X64InstrKind_SUB_RI);

    //u32 stack_size = X64__assign_proc_stack_offsets(&state);

    //////////////////////////////////////////////////////////////////////////////////////////
    allocator_restore_state(tmp_mem_state);

    return state.instrs;
}
