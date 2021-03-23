#include "allocator.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct MemBlockFooter {
    unsigned char* pbuffer;
    unsigned char* pend;
} MemBlockFooter;

static bool alloc_mem_block(Allocator* allocator, size_t size)
{
    // Adds a new block of memory to the allocator.
    // The new block will contain a pointer to the old block in its footer (for cleanup).
    // The block size doubles every time.

    size_t block_size = allocator->block_size;
    if (block_size < size) {
        block_size = size;
    }
    block_size *= 2;

    unsigned char* block = malloc(block_size + sizeof(MemBlockFooter));
    if (!block) {
        return false;
    }

    unsigned char* pbuffer = allocator->buffer;
    unsigned char* pend = allocator->end;

    allocator->buffer = block;
    allocator->end = block + block_size;
    allocator->at = block + size;
    allocator->block_size = block_size;

    MemBlockFooter* footer = (MemBlockFooter*)allocator->end;
    footer->pbuffer = pbuffer;
    footer->pend = pend;

    return true;
}

void* mem_allocate(Allocator* allocator, size_t size, size_t align, bool clear)
{
    assert(align > 0);
    assert((align & (align - 1)) == 0);
    if (!allocator) {
        return clear ? calloc(1, size) : malloc(size);
    }

    void* memory = NULL;

    uintptr_t aligned_at = (((uintptr_t)allocator->at) + align - 1) & ~(align - 1);
    uintptr_t new_at = aligned_at + size;

    // Allocate a new memory block if need more memory.
    if (new_at > (uintptr_t)allocator->end) {
        if (!alloc_mem_block(allocator, size)) {
            return NULL;
        }

        aligned_at = (((uintptr_t)allocator->at) + align - 1) & ~(align - 1);
        new_at = aligned_at + size;
    }

    memory = (void*)aligned_at;
    allocator->at = (unsigned char*)new_at;

    if (clear) {
        memset(memory, 0, size);
    }

    return memory;
}

void* mem_dup(Allocator* allocator, void* src, size_t size, size_t align)
{
    void* memory = mem_allocate(allocator, size, align, false);

    memcpy(memory, src, size);

    return memory;
}

void* mem_reallocate(Allocator* allocator, void* ptr, size_t old_size, size_t size, size_t align)
{
    if (!allocator) {
        return realloc(ptr, size);
    }

    // TODO: If this allocation is at the top of the arena stack, allocation is not necessary!
    void* memory = mem_allocate(allocator, size, align, false);

    if (memory && ptr) {
        memcpy(memory, ptr, old_size);
    }

    return memory;
}

void mem_free(Allocator* allocator, void* ptr)
{
    if (!allocator) {
        free(ptr);
    }
    // Do nothing.
    // TODO: If ptr is the last previous allocation, can undo it.
}

Allocator allocator_create(size_t block_size)
{
    static size_t id = 0;
    Allocator allocator = {0};
    allocator.id = id++;
    allocator.block_size = block_size;

    return allocator;
}

void allocator_reset(Allocator* allocator)
{
    if (!allocator->buffer) {
        return;
    }

    // Free all blocks, except for the last (original), which is "cleared".
    MemBlockFooter* footer = (MemBlockFooter*)allocator->end;

    while (footer->pbuffer) {
        unsigned char* pbuffer = footer->pbuffer;
        unsigned char* pend = footer->pend;

        free(allocator->buffer);

        allocator->buffer = pbuffer;
        allocator->end = pend;
        footer = (MemBlockFooter*)allocator->end;
    }

    allocator->at = allocator->buffer;
}

void allocator_destroy(Allocator* allocator)
{
    if (!allocator->buffer) {
        return;
    }

    unsigned char* buffer = allocator->buffer;
    unsigned char* end = allocator->end;

    while (buffer) {
        MemBlockFooter footer = *(MemBlockFooter*)(end);

        free(buffer);

        buffer = footer.pbuffer;
        end = footer.pend;
    }

    allocator->buffer = NULL;
    allocator->at = NULL;
    allocator->end = NULL;
}

AllocatorState allocator_get_state(Allocator* allocator)
{
    AllocatorState state = {0};
    state.allocator = allocator;
    state.buffer = allocator->buffer;
    state.at = allocator->at;

    return state;
}

void allocator_restore_state(AllocatorState state)
{
    Allocator* allocator = state.allocator;
    unsigned char* dest_buffer = state.buffer;
    unsigned char* buffer = allocator->buffer;
    unsigned char* end = allocator->end;

    while (buffer != dest_buffer) {
        MemBlockFooter footer = *(MemBlockFooter*)(end);

        free(buffer);

        buffer = footer.pbuffer;
        end = footer.pend;
    }

    allocator->buffer = buffer;
    allocator->end = end;
    allocator->at = state.at;
}
