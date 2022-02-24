#include "x64_gen/reg_alloc.h"

typedef struct X64_IntervalList {
    List list;
    u32 count;
} X64_IntervalList;

typedef struct X64_RegAllocState {
    // Doubly-linked list of currently active intervals (sorted by increasing end).
    X64_IntervalList active;

    // Doubly-linked list of currently unhandled intervals (sorted by increasing start).
    X64_IntervalList unhandled;

    // Doubly-linked list of handled intervals (sorted by increasing start).
    X64_IntervalList handled;

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

static X64_Reg X64_next_reg(X64_RegAllocState* state, u32 lreg, u32 banned_regs, X64_Reg preg_hint)
{
    // Try to allocate hint register first if:
    // - Provided a hint register
    // - Hint register is not banned
    // - Hint register is available
    if ((preg_hint != X64_REG_COUNT) && !u32_is_bit_set(banned_regs, preg_hint) && (state->rmap[preg_hint] == (u32)-1)) {
        X64_alloc_reg(state, preg_hint, lreg);

        return preg_hint;
    }

    // Get the first available scratch register.
    X64_Reg reg = X64_REG_COUNT;

    u32 nregs = state->num_scratch_regs;
    X64_Reg* regs = state->scratch_regs;

    for (u32 i = 0; i < nregs; i += 1) {
        X64_Reg r = regs[i];

        // Try to get a physical register that is free and is not banned.
        if ((state->rmap[r] == (u32)-1) && !u32_is_bit_set(banned_regs, r)) {
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

static void X64_lreg_interval_list_rm(X64_IntervalList* list, X64_LRegRange* interval)
{
    assert(!list_empty(&list->list));
    assert(list->count > 0);

    list_rm(&interval->lnode);

    list->count -= 1;
}

typedef enum X64_IntervalSortKind {
    X64_INTERVAL_SORT_START = 0,
    X64_INTERVAL_SORT_END,
} X64_IntervalSortKind;

static void X64_lreg_interval_list_add(X64_IntervalList* list, X64_LRegRange* interval, X64_IntervalSortKind sort)
{
    // Insert sorted by increasing start or end location.
    List* head = &list->list;
    List* it = head->next;

    while (it != head) {
        X64_LRegRange* it_entry = list_entry(it, X64_LRegRange, lnode);

        if ((sort == X64_INTERVAL_SORT_END) && (interval->end < it_entry->end)) {
            break;
        }
        else if ((sort == X64_INTERVAL_SORT_START) && (interval->start < it_entry->start)) {
            break;
        }

        it = it->next;
    }

    // Insert before `it`
    list_add(it->prev, &interval->lnode);

    list->count += 1;
}

static void X64_spill_reg_loc(X64_RegAllocState* state, X64_LRegRange* interval)
{
    state->result.stack_offset += X64_MAX_INT_REG_SIZE;

    interval->loc.kind = X64_LREG_LOC_STACK;
    interval->loc.offset = -state->result.stack_offset;

    // Add to `handled` set
    X64_lreg_interval_list_add(&state->handled, interval, X64_INTERVAL_SORT_START);
}

static void X64_steal_reg(X64_RegAllocState* state, X64_LRegRange* from, X64_LRegRange* to)
{
    u32 to_lreg = to->lreg;
    X64_LRegLoc* f_loc = &from->loc;
    X64_LRegLoc* t_loc = &to->loc;

    assert(f_loc->kind == X64_LREG_LOC_REG);

    // Steal from's register.
    t_loc->kind = X64_LREG_LOC_REG;
    t_loc->reg = f_loc->reg;
    state->rmap[t_loc->reg] = to_lreg; // Update reg alloc map

    // Update active list
    X64_lreg_interval_list_rm(&state->active, from);
    X64_lreg_interval_list_add(&state->active, to, X64_INTERVAL_SORT_END);

    // Spill from's interval to the stack.
    X64_spill_reg_loc(state, from);
}

// Modified linear scan register allocation adapted from Poletto et al (1999)
//
// Assumptions:
//  - One interval per LIR register.
//
// This register allocator is pretty basic and not very good, but we just need something that works for now.
//
X64_RegAllocResult X64_linear_scan_reg_alloc(X64_LIRBuilder* builder, u32 num_x64_regs, X64_Reg* x64_scratch_regs,
                                             u32 init_stack_offset)
{
    X64_RegAllocState state = {.num_scratch_regs = num_x64_regs,
                               .scratch_regs = x64_scratch_regs,
                               .result = {.stack_offset = init_stack_offset}};

    X64_init_free_regs(&state);
    list_head_init(&state.active.list);
    list_head_init(&state.unhandled.list);
    list_head_init(&state.handled.list);

    // Add intervals to the `unhandled` list (sorterd by increasing start)
    size_t num_lreg_ranges = array_len(builder->lreg_ranges);
    size_t num_intervals = 0;

    for (size_t i = 0; i < num_lreg_ranges; i += 1) {
        if (X64_find_alias_reg(builder, i) != i) {
            continue;
        }

        X64_lreg_interval_list_add(&state.unhandled, builder->lreg_ranges + i, X64_INTERVAL_SORT_START);
        num_intervals += 1;
    }

    List* uhead = &state.unhandled.list;
    List* uit = uhead->next;

    while (uit != uhead) {
        List* unext = uit->next;
        X64_LRegRange* interval = list_entry(uit, X64_LRegRange, lnode);

        X64_lreg_interval_list_rm(&state.unhandled, interval);

        // Expire old intervals
        {
            List* head = &state.active.list;
            List* it = head->next;

            while (it != head) {
                List* next = it->next;

                X64_LRegRange* it_entry = list_entry(it, X64_LRegRange, lnode);

                if (it_entry->end > interval->start) {
                    break;
                }

                // This active interval ends before (or at) the current interval.
                //
                // Remove active interval from active list and free its register.
                X64_lreg_interval_list_rm(&state.active, it_entry);

                X64_LRegLoc* loc = &it_entry->loc;

                assert(loc->kind == X64_LREG_LOC_REG);
                X64_free_reg(&state, loc->reg);

                // Add to `handled` list
                X64_lreg_interval_list_add(&state.handled, it_entry, X64_INTERVAL_SORT_START);

                it = next;
            }
        }

        if (interval->ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_REG) {
            //
            // Interval is forced to reside in a specific register.
            //
            X64_Reg forced_reg = interval->ra_ctrl.preg;

            assert(state.rmap[forced_reg] == (u32)-1);

            interval->loc.kind = X64_LREG_LOC_REG;
            interval->loc.reg = forced_reg;

            X64_alloc_reg(&state, forced_reg, interval->lreg);
            X64_lreg_interval_list_add(&state.active, interval, X64_INTERVAL_SORT_END);
        }
        else if (interval->ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_ANY_REG) {
            //
            // Interval is forced to reside in a register.
            //

            assert(interval->ra_ctrl.preg_mask);

            X64_Reg reg = X64_next_reg(&state, interval->lreg, ~interval->ra_ctrl.preg_mask, X64_REG_COUNT);

            assert(reg != X64_REG_COUNT);
            interval->loc.kind = X64_LREG_LOC_REG;
            interval->loc.reg = reg;
            X64_lreg_interval_list_add(&state.active, interval, X64_INTERVAL_SORT_END);
        }
        else if (interval->ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL) {
            assert(interval->ra_ctrl.preg_mask);
            
            X64_Reg reg = X64_next_reg(&state, interval->lreg, ~interval->ra_ctrl.preg_mask, X64_REG_COUNT);

            if (reg != X64_REG_COUNT) {
                interval->loc.kind = X64_LREG_LOC_REG;
                interval->loc.reg = reg;
                X64_lreg_interval_list_add(&state.active, interval, X64_INTERVAL_SORT_END);
            }
            else {
                X64_spill_reg_loc(&state, interval);
            }
        }
        else {
            //
            // Check if need to spill OR if can allocate a register.
            //

            // Scan forward to determine which registers we cannot use
            // (due to future intersecting intervals that are forced into regs)
            u32 banned_regs = 0;

            List* jit = unext;

            while (jit != uhead) {
                X64_LRegRange* j_rng = list_entry(jit, X64_LRegRange, lnode);

                // Stop scanning once intervals no longer intersect. (or just intersect at endpoints)
                if (j_rng->start >= interval->end) {
                    break;
                }

                // Add this interval's forced register to the bit-set of registers the current interval cannot use.
                if (j_rng->ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_REG) {
                    banned_regs |= j_rng->ra_ctrl.preg_mask;
                }

                jit = jit->next;
            }

            X64_RegAllocControlKind ra_ctrl_kind = interval->ra_ctrl_kind;

            if (state.active.count == state.num_scratch_regs) {
                // Exhausted all available free registers. Spill the longest interval that IS NOT forced into a register.

                bool force_reg = (ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_REG) || (ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_ANY_REG);

                // Look for the latest active interval that is not forced into a register and is not using a banned register.
                List* head = &state.active.list;
                List* it = head->prev;

                for (; it != head; it = it->prev) {
                    X64_LRegRange* it_e = list_entry(it, X64_LRegRange, lnode);
                    if (it_e->ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_REG ||
                        it_e->ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_ANY_REG) continue;

                    bool using_banned_reg = (it_e->loc.kind == X64_LREG_LOC_REG) && u32_is_bit_set(banned_regs, it_e->loc.reg);
                    if (!using_banned_reg) break;
                }

                if ((it == head) && force_reg) {
                    // All other active intervals are forced into registers, and we are not able to spill this one.
                    // Fail & exit.
                    state.result.success = false;
                    return state.result;
                }

                if (it == head) {
                    // All other active intervals are forced into registers, so spill this one.
                    X64_spill_reg_loc(&state, interval);
                }
                else {
                    // If forcing this interval into a register, steal a register from another interval (longest end).
                    // Otherwise, spill the interval that ends the latest.
                    X64_LRegRange* last_active = list_entry(it, X64_LRegRange, lnode);

                    if (force_reg || (last_active->end > interval->end)) {
                        X64_steal_reg(&state, last_active, interval);
                    }
                    else {
                        X64_spill_reg_loc(&state, interval);
                    }
                }
            }
            else {
                X64_Reg preg_hint = X64_REG_COUNT;

                // Extract register hint, if available.
                if (ra_ctrl_kind == X64_REG_ALLOC_CTRL_HINT_PHYS_REG) {
                    preg_hint = interval->ra_ctrl.preg;
                    assert(preg_hint < X64_REG_COUNT);
                }
                else if (ra_ctrl_kind == X64_REG_ALLOC_CTRL_HINT_LIR_REG) {
                    u32 lreg_hint = interval->ra_ctrl.lreg;
                    assert(lreg_hint < builder->num_regs);

                    X64_LRegLoc* hint_loc = &builder->lreg_ranges[lreg_hint].loc;

                    if (hint_loc->kind == X64_LREG_LOC_REG) {
                        preg_hint = hint_loc->reg;
                        assert(preg_hint < X64_REG_COUNT);
                    }
                }

                // Try to allocate the next free reg.
                X64_Reg reg = X64_next_reg(&state, interval->lreg, banned_regs, preg_hint);

                if (reg == X64_REG_COUNT) {
                    state.result.success = false;
                    return state.result;
                }

                interval->loc.kind = X64_LREG_LOC_REG;
                interval->loc.reg = reg;
                X64_lreg_interval_list_add(&state.active, interval, X64_INTERVAL_SORT_END);
            }
        }

        uit = unext;
    }



    // Process call sites to generate push/pop regs
    List* head = &state.active.list;
    List* it = head->next;

    while (it != head) {
        List* next = it->next;
        X64_LRegRange* interval = list_entry(it, X64_LRegRange, lnode);

        X64_lreg_interval_list_rm(&state.active, interval);
        X64_lreg_interval_list_add(&state.handled, interval, X64_INTERVAL_SORT_START);

        it = next;
    }

    assert(num_intervals == state.handled.count);

    size_t num_sites = array_len(builder->call_sites);
    X64_Instr** call_sites = builder->call_sites;
    head = &state.handled.list;
    it = head->next;
    
    for (size_t i = 0; i < num_sites; i++) {
        X64_Instr* instr = call_sites[i];
        long ino = instr->ino;
        unsigned* save_reg_mask;

        if (instr->kind == X64_INSTR_CALL) {
            save_reg_mask = &instr->call.save_reg_mask;
        }
        else {
            assert(instr->kind == X64_INSTR_CALL_R);
            save_reg_mask = &instr->call_r.save_reg_mask;
        }

        *save_reg_mask = 0;

        // Scan forward to first interval that intersects with call site.
        bool intersects = false;

        while (it != head) {
            X64_LRegRange* interval = list_entry(it, X64_LRegRange, lnode);

            // First interval past the call site
            if (interval->start >= ino) {
                break;
            }

            // Intersects
            if ((interval->start < ino) && (ino < interval->end)) {
                X64_LRegLoc* loc = &interval->loc;

                if (loc->kind == X64_LREG_LOC_REG && X64_is_caller_saved_reg(loc->reg)) {
                    intersects = true;
                    break;
                }
            }

            it = it->next;
        }

        if (!intersects) {
            continue;
        }

        // Scan forward to accumulate registers that need to be saved across the call site.
        List* jit = it;

        while (jit != head) {
            X64_LRegRange* interval = list_entry(jit, X64_LRegRange, lnode);

            if (interval->start >= ino) {
                break;
            }

            if ((interval->start < ino) && (ino < interval->end)) {
                X64_LRegLoc* loc = &interval->loc;

                if (loc->kind == X64_LREG_LOC_REG && X64_is_caller_saved_reg(loc->reg)) {
                    assert(loc->reg < X64_REG_COUNT);
                    *save_reg_mask |= (1 << loc->reg);
                }
            }

            jit = jit->next;
        }
    }

    state.result.stack_offset = ALIGN_UP(state.result.stack_offset, X64_STACK_ALIGN);
    state.result.success = true;

    return state.result;
}
