#include "x64_gen/reg_alloc.h"

static void X64_free_reg(u32* free_regs, X64_Reg reg)
{
    u32_set_bit(free_regs, (u8)reg);
}

static void X64_alloc_reg(u32* free_regs, u32* used_callee_regs, X64_Reg reg)
{
    u32_unset_bit(free_regs, (u8)reg);

    if (X64_is_reg_callee_saved(reg))
    {
        u32_set_bit(used_callee_regs, (u8)reg);
    }
}

static X64_Reg X64_next_reg(u32 num_x64_regs, X64_Reg* x64_scratch_regs, u32* free_regs, u32* used_callee_regs,
                            bool is_ret, bool is_arg, u32 arg_index)
{
    // Try to allocate RAX if this is a return value.
    if (is_ret)
    {
        X64_Reg ret_reg = X64_RAX;

        if (u32_is_bit_set(*free_regs, (u8)ret_reg))
        {
            X64_alloc_reg(free_regs, used_callee_regs, ret_reg);
            return ret_reg;
        }
    }

    // Try to allocate an argument register if available.
    if (is_arg && (arg_index < X64_NUM_ARG_REGS))
    {
        X64_Reg arg_reg = x64_arg_regs[arg_index];

        if (u32_is_bit_set(*free_regs, (u8)arg_reg))
        {
            X64_alloc_reg(free_regs, used_callee_regs, arg_reg);
            return arg_reg;
        }
    }

    // Get the first available scratch register.
    X64_Reg reg = X64_REG_COUNT;

    for (u32 i = 0; i < num_x64_regs; i += 1)
    {
        if (u32_is_bit_set(*free_regs, (u8)x64_scratch_regs[i]))
        {
            reg = x64_scratch_regs[i];
            break;
        }
    }

    // Do not assign an argument register to an argument that should be passed via the stack.
    if (is_arg && (arg_index >= X64_NUM_ARG_REGS) && u32_is_bit_set(X64_ARG_REG_MASK, reg))
    {
        reg = X64_REG_COUNT;
    }

    if (reg != X64_REG_COUNT)
    {
        X64_alloc_reg(free_regs, used_callee_regs, reg);
    }

    return reg;
}

static void X64_init_free_regs(u32 num_x64_regs, X64_Reg* x64_scratch_regs, u32* free_regs)
{
    for (u32 i = 0; i < num_x64_regs; i += 1)
    {
        X64_free_reg(free_regs, x64_scratch_regs[i]);
    }
}

void X64_vreg_interval_list_rm(X64_VRegIntervalList* list, X64_VRegInterval* node)
{
    assert(node != &list->sentinel);

    X64_VRegInterval* prev = node->prev;
    X64_VRegInterval* next = node->next;

    prev->next = next;
    next->prev = prev;

    node->next = node->prev = NULL;

    list->count -= 1;
}

void X64_vreg_interval_list_add(X64_VRegIntervalList* list, LifetimeInterval* interval, u32 index)
{
    X64_VRegInterval* new_node = alloc_type(list->arena, X64_VRegInterval, true);
    new_node->interval = *interval;
    new_node->index = index;

    // Insert sorted by increasing end point.

    X64_VRegInterval* head = &list->sentinel;
    X64_VRegInterval* it = head->next;

    while (it != head)
    {
        if (new_node->interval.end < it->interval.end)
        {
            break;
        }

        it = it->next;
    }

    // Insert before `it`
    X64_VRegInterval* prev = it->prev;

    prev->next = new_node;
    new_node->prev = prev;
    new_node->next = it;
    it->prev = new_node;

    list->count += 1;
}

X64_RegAllocResult X64_linear_scan_reg_alloc(Allocator* arena, u32 num_vregs, LifetimeInterval* vreg_intervals,
                                             X64_VRegLoc* vreg_locs, u32 num_x64_regs, X64_Reg* x64_scratch_regs,
                                             u32 init_stack_offset)
{
    X64_RegAllocResult result = {.stack_offset = init_stack_offset};
    X64_init_free_regs(num_x64_regs, x64_scratch_regs, &result.free_regs);

    X64_VRegIntervalList active = {.arena = arena};
    active.sentinel.next = &active.sentinel;
    active.sentinel.prev = &active.sentinel;

    for (size_t i = 0; i < num_vregs; i += 1)
    {
        LifetimeInterval* interval = vreg_intervals + i;

        // Expire old intervals
        {
            X64_VRegInterval* head = &active.sentinel;
            X64_VRegInterval* it = head->next;

            while (it != head)
            {
                X64_VRegInterval* next = it->next;

                if (it->interval.end >= interval->start)
                {
                    break;
                }

                // This active interval ends before the current interval.
                //
                // Remove active interval from active list and free its register.
                X64_vreg_interval_list_rm(&active, it);

                X64_VRegLoc* loc = vreg_locs + it->index;

                if (loc->kind == X64_VREG_LOC_REG)
                {
                    X64_free_reg(&result.free_regs, loc->reg);
                }

                it = next;
            }
        }

        // Check if need to spill
        if (active.count == num_x64_regs)
        {
            X64_VRegInterval* last_active = active.sentinel.prev;

            // Spill interval that ends the latest
            if (last_active->interval.end > interval->end)
            {
                // Steal last_active's register.
                vreg_locs[i].kind = X64_VREG_LOC_REG;
                vreg_locs[i].reg = vreg_locs[last_active->index].reg;

                // Spill the last active interval.
                vreg_locs[last_active->index].kind = X64_VREG_LOC_STACK;
                vreg_locs[last_active->index].offset = result.stack_offset;
                result.stack_offset += X64_MAX_INT_REG_SIZE;

                X64_vreg_interval_list_rm(&active, last_active);
                X64_vreg_interval_list_add(&active, interval, i);
            }
            else
            {
                vreg_locs[i].kind = X64_VREG_LOC_STACK;
                vreg_locs[i].offset = result.stack_offset;
                result.stack_offset += X64_MAX_INT_REG_SIZE;
            }
        }
        else
        {
            // Try to allocate the next free reg. This will only fail if this interval corresponds to a procedure call argument
            // AND 1) the argument should be passed via the stack and 2) we can only assign an X64 argument register.
            //
            // We want to prevent arguments that should be passed via the stack from being assigned an argument register.
            // Refer to the logic for emitting X64 call instructions for more info.
            X64_Reg reg = X64_next_reg(num_x64_regs, x64_scratch_regs, &result.free_regs, &result.used_callee_regs,
                                       interval->is_ret, interval->is_arg, interval->arg_index);

            if (reg != X64_REG_COUNT)
            {
                vreg_locs[i].kind = X64_VREG_LOC_REG;
                vreg_locs[i].reg = reg;

                X64_vreg_interval_list_add(&active, interval, i);
            }
            else
            {
                // This is a call argument that we refused to assign to a free register.
                vreg_locs[i].kind = X64_VREG_LOC_STACK;
                vreg_locs[i].offset = result.stack_offset;
                result.stack_offset += X64_MAX_INT_REG_SIZE;
            }
        }
    }

    result.stack_offset = ALIGN_UP(result.stack_offset, X64_STACK_ALIGN);

    return result;
}
