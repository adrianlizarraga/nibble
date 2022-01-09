#include "x64_gen/reg_alloc.h"

typedef struct X64_RegAllocState {
    // Doubly-linked list of currently active intervals (sorted by increasing end).
    List active;
    u32 num_active;

    // Subset of the physical registers to actually use for allocation.
    // We don't want to use RBP or RSP, for example, as general registers.
    u32 num_scratch_regs;
    X64_Reg* scratch_regs;

    // Maps a physical register into the virtual register/interval that occupies it.
    // A value of all ones (0xFFFFFFFF) means unoccupied.
    u32 rmap[X64_REG_COUNT];

    X64_RegAllocResult result;
} X64_RegAllocState;

static void X64_free_reg(X64_RegAllocState* state, X64_Reg reg)
{
    state->rmap[reg] = (u32)-1;
}

static void X64_alloc_reg(X64_RegAllocState* state, X64_Reg reg, u32 lreg)
{
    state->rmap[reg] = lreg;

    if (X64_is_callee_saved_reg(reg)) {
        u32_set_bit(&state->result.used_callee_regs, (u8)reg);
    }
}

// static X64_Reg X64_next_reg(u32 num_x64_regs, X64_Reg* x64_scratch_regs, u32* free_regs, u32* used_callee_regs)
static X64_Reg X64_next_reg(X64_RegAllocState* state, u32 lreg)
{
    // Get the first available scratch register.
    X64_Reg reg = X64_REG_COUNT;

    u32 nregs = state->num_scratch_regs;
    X64_Reg* regs = state->scratch_regs;

    for (u32 i = 0; i < nregs; i += 1) {
        X64_Reg r = regs[i];

        if (state->rmap[r] == (u32)-1) {
            reg = r;
            break;
        }
    }

    if (reg != X64_REG_COUNT) {
        X64_alloc_reg(state, reg, lreg);
    }

    assert(reg != X64_RBP);
    assert(reg != X64_RSP);

    return reg;
}

static void X64_init_free_regs(X64_RegAllocState* state)
{
    for (size_t i = 0; i < X64_REG_COUNT; i++)
        state->rmap[i] = (u32)-1;
}

static void X64_lreg_interval_list_rm(X64_RegAllocState* state, X64_LRegRange* interval)
{
    assert(!list_empty(&state->active));
    assert(state->num_active > 0);

    list_rm(&interval->lnode);

    state->num_active -= 1;
}

static void X64_lreg_interval_list_add(X64_RegAllocState* state, X64_LRegRange* interval)
{
    // Insert sorted by increasing end point.
    List* head = &state->active;
    List* it = head->next;

    while (it != head) {
        X64_LRegRange* it_entry = list_entry(it, X64_LRegRange, lnode);

        if (interval->end < it_entry->end) {
            break;
        }

        it = it->next;
    }

    // Insert before `it`
    list_add(it->prev, &interval->lnode);

    state->num_active += 1;
}

static void X64_spill_reg_loc(X64_RegAllocState* state, X64_LRegLoc* loc)
{
    loc->kind = X64_LREG_LOC_STACK;
    loc->offset = -state->result.stack_offset;

    state->result.stack_offset += X64_MAX_INT_REG_SIZE;
}

static void X64_steal_reg(X64_RegAllocState* state, X64_LRegRange* from, X64_LRegRange* to, u32 to_lreg)
{
    X64_LRegLoc* f_loc = &from->loc;
    X64_LRegLoc* t_loc = &to->loc;

    assert(f_loc->kind == X64_LREG_LOC_REG);

    // Steal from's register.
    t_loc->kind = X64_LREG_LOC_REG;
    t_loc->reg = f_loc->reg;
    state->rmap[t_loc->reg] = to_lreg; // Update reg alloc map

    // Spill from's interval to the stack.
    X64_spill_reg_loc(state, f_loc);

    // Update active list
    X64_lreg_interval_list_rm(state, from);
    X64_lreg_interval_list_add(state, to);
}

// Modified linear scan register allocation adapted from Poletto et al (1999)
//
// Assumptions:
//  - One interval per LIR register.
//  - SSA (each LIR register is set only once)
//  - "Single-use": Each LIR register is only used once! This will have to change once we do properly SSA-based optimizations.
//
// This register allocator is pretty basic and not very good, but we just need something that works for now.
//
// NOTE: LIR registers needed across procedure calls will NOT be assigned a physical register.
X64_RegAllocResult X64_linear_scan_reg_alloc(X64_LIRBuilder* builder, u32 num_x64_regs, X64_Reg* x64_scratch_regs,
                                             u32 init_stack_offset)
{
    X64_RegAllocState state = {.num_scratch_regs = num_x64_regs,
                               .scratch_regs = x64_scratch_regs,
                               .result = {.stack_offset = init_stack_offset}};

    X64_init_free_regs(&state);
    list_head_init(&state.active);

    size_t num_lreg_ranges = array_len(builder->lreg_ranges);
    size_t call_idx = 0;

    for (size_t i = 0; i < num_lreg_ranges; i += 1) {
        // Skip intervals for registers that have been aliased to another register.
        if (X64_find_alias_reg(builder, i) != i) {
            continue;
        }

        X64_LRegRange* interval = builder->lreg_ranges + i;

        // Expire old intervals
        {
            List* head = &state.active;
            List* it = head->next;

            while (it != head) {
                List* next = it->next;

                X64_LRegRange* it_entry = list_entry(it, X64_LRegRange, lnode);

                if (it_entry->end >= interval->start) {
                    break;
                }

                // This active interval ends before (or at) the current interval.
                //
                // Remove active interval from active list and free its register.
                X64_lreg_interval_list_rm(&state, it_entry);

                X64_LRegLoc* loc = &it_entry->loc;

                assert(loc->kind == X64_LREG_LOC_REG);
                X64_free_reg(&state, loc->reg);

                it = next;
            }
        }

        // Set `call_idx` to the index of the next upcoming call site.
        while (builder->call_sites[call_idx] < interval->start) {
            call_idx++;
        }

        u32 call_site = builder->call_sites[call_idx];

        //
        // Check if need to spill OR if can allocate a register.
        //

        if (interval->loc.kind == X64_LREG_LOC_REG) {
            //
            // Interval is forced to reside in a specific register.
            //
            assert(interval->loc.reg != X64_REG_COUNT);

            u32 assigned_lreg = state.rmap[interval->loc.reg];

            if (assigned_lreg != (u32)-1) {
                X64_LRegRange* steal_from = builder->lreg_ranges + assigned_lreg;

                // TODO: NEED TO DO BETTER. This likely unnecessarily spills an interval that just
                // could have used a different register.
                //
                // Potential fix: Keep a "forward" bitset of intersecting forced-reg intervals that should NOT 
                // be used by current interval.
                X64_steal_reg(&state, steal_from, interval, i);
            }
            else {
                X64_alloc_reg(&state, interval->loc.reg, i);
                X64_lreg_interval_list_add(&state, interval);
            }
        }
        else if ((interval->start < call_site) && (interval->end > call_site)) {
            // Spill any intervals needed across procedure calls. (For simplicity)
            X64_spill_reg_loc(&state, &interval->loc);
        }
        else if (state.num_active == state.num_scratch_regs) {
            // Exhausted available registers. Spill the longest interval.

            X64_LRegRange* last_active = list_entry(state.active.prev, X64_LRegRange, lnode);

            // Spill interval that ends the latest
            if (last_active->end > interval->end) {
                X64_steal_reg(&state, last_active, interval, i);
            }
            else {
                X64_spill_reg_loc(&state, &interval->loc);
            }
        }
        else {
            // Try to allocate the next free reg.
            X64_Reg reg = X64_next_reg(&state, i);

            assert(reg != X64_REG_COUNT);
            interval->loc.kind = X64_LREG_LOC_REG;
            interval->loc.reg = reg;
            X64_lreg_interval_list_add(&state, interval);
        }
    }

    state.result.stack_offset = ALIGN_UP(state.result.stack_offset, X64_STACK_ALIGN);

    return state.result;
}
