#include "x64_gen/reg_alloc.h"

typedef struct X64_LRegInterval {
    X64_LRegRange* interval;
    struct X64_LRegInterval* next;
    struct X64_LRegInterval* prev;
} X64_LRegInterval;

typedef struct X64_LRegIntervalList {
    X64_LRegInterval sentinel;
    u32 count;
    Allocator* arena;
} X64_LRegIntervalList;

static void X64_free_reg(u32* free_regs, X64_Reg reg)
{
    u32_set_bit(free_regs, (u8)reg);
}

static void X64_alloc_reg(u32* free_regs, u32* used_callee_regs, X64_Reg reg)
{
    u32_unset_bit(free_regs, (u8)reg);

    if (X64_is_callee_saved_reg(reg)) {
        u32_set_bit(used_callee_regs, (u8)reg);
    }
}

static X64_Reg X64_next_reg(u32 num_x64_regs, X64_Reg* x64_scratch_regs, u32* free_regs, u32* used_callee_regs)
{
    // Get the first available scratch register.
    X64_Reg reg = X64_REG_COUNT;

    for (u32 i = 0; i < num_x64_regs; i += 1) {
        if (u32_is_bit_set(*free_regs, (u8)x64_scratch_regs[i])) {
            reg = x64_scratch_regs[i];
            break;
        }
    }

    if (reg != X64_REG_COUNT) {
        X64_alloc_reg(free_regs, used_callee_regs, reg);
    }

    return reg;
}

static void X64_init_free_regs(u32 num_x64_regs, X64_Reg* x64_scratch_regs, u32* free_regs)
{
    (void)num_x64_regs;
    (void)x64_scratch_regs;
    *free_regs = (u32)-1;
}

static void X64_lreg_interval_list_rm(X64_LRegIntervalList* list, X64_LRegInterval* node)
{
    assert(node != &list->sentinel);

    X64_LRegInterval* prev = node->prev;
    X64_LRegInterval* next = node->next;

    prev->next = next;
    next->prev = prev;

    node->next = node->prev = NULL;

    list->count -= 1;
}

static void X64_lreg_interval_list_add(X64_LRegIntervalList* list, X64_LRegRange* interval)
{
    X64_LRegInterval* new_node = alloc_type(list->arena, X64_LRegInterval, true);
    new_node->interval = interval;

    // Insert sorted by increasing end point.

    X64_LRegInterval* head = &list->sentinel;
    X64_LRegInterval* it = head->next;

    while (it != head) {
        if (new_node->interval.end < it->interval.end) {
            break;
        }

        it = it->next;
    }

    // Insert before `it`
    X64_LRegInterval* prev = it->prev;

    prev->next = new_node;
    new_node->prev = prev;
    new_node->next = it;
    it->prev = new_node;

    list->count += 1;
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
X64_RegAllocResult X64_linear_scan_reg_alloc(X64_LIRBuilder* builder, u32 num_x64_regs, X64_Reg* x64_scratch_regs, u32 init_stack_offset)
{
    X64_RegAllocResult result = {.stack_offset = init_stack_offset};
    X64_init_free_regs(num_x64_regs, x64_scratch_regs, &result.free_regs);

    X64_LRegIntervalList active = {.arena = builder->arena};
    active.sentinel.next = &active.sentinel;
    active.sentinel.prev = &active.sentinel;

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
            X64_LRegInterval* head = &active.sentinel;
            X64_LRegInterval* it = head->next;

            while (it != head) {
                X64_LRegInterval* next = it->next;

                if (it->interval.end > interval->start) {
                    break;
                }

                // This active interval ends before (or at) the current interval.
                //
                // Remove active interval from active list and free its register.
                X64_lreg_interval_list_rm(&active, it);

                X64_LRegLoc* loc = &it->interval->loc;

                if (loc->kind == X64_LREG_LOC_REG) {
                    X64_free_reg(&result.free_regs, loc->reg);
                }

                it = next;
            }
        }

        // Set `call_idx` to the index of the next upcoming call site.
        while (builder->call_sites[call_idx] < interval->start) {
            call_idx++;
        }

        //
        // Check if need to spill OR if can allocate a register.
        //
        
        if (interval->loc.kind == X64_LREG_LOC_REG) {
            //
            // Interval is forced to reside in a specific register.
            //
            assert(interval->loc.reg != X64_REG_COUNT);
            assert(u32_is_bit_set(result.free_regs, interval->loc.reg));
            
            X64_alloc_reg(&result.free_regs, &result.used_callee_regs, interval->loc.reg);
            X64_lreg_interval_list_add(&active, interval);
        }
        else if (interval->end > builder->call_sites[call_idx]) {
            //
            // Spill any intervals needed across procedure calls. (For simplicity)
            //
            X64_LRegLoc* loc = &interval->loc;

            loc->kind = X64_LREG_LOC_STACK;
            loc->offset = result.stack_offset;
            result.stack_offset += X64_MAX_INT_REG_SIZE;
        }
        else if (active.count == num_x64_regs) {
            //
            // Exhausted available registers.
            //

            X64_LRegInterval* last_active = active.sentinel.prev;
            X64_LRegLoc* loc = interval->loc;

            // Spill interval that ends the latest
            if (last_active->interval.end > interval->end) {
                X64_LRegLoc* p_loc = &last_active->interval->loc;

                // Steal last_active's register.
                loc->kind = X64_LREG_LOC_REG;
                loc->reg = p_loc->reg;

                // Spill the last active interval.
                p_loc->kind = X64_LREG_LOC_STACK;
                p_loc->offset = result.stack_offset;
                result.stack_offset += X64_MAX_INT_REG_SIZE;

                X64_lreg_interval_list_rm(&active, last_active);
                X64_lreg_interval_list_add(&active, interval);
            }
            else {
                loc->kind = X64_LREG_LOC_STACK;
                loc->offset = result.stack_offset;
                result.stack_offset += X64_MAX_INT_REG_SIZE;
            }
        }
        else {
            // Try to allocate the next free reg.
            X64_Reg reg = X64_next_reg(num_x64_regs, x64_scratch_regs, &result.free_regs, &result.used_callee_regs);

            assert(reg != X64_REG_COUNT);
            interval->loc.kind = X64_LREG_LOC_REG;
            interval->loc.reg = reg;

            X64_lreg_interval_list_add(&active, interval);
        }
    }

    result.stack_offset = ALIGN_UP(result.stack_offset, X64_STACK_ALIGN);

    return result;
}
