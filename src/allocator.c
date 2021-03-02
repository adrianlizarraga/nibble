#include "allocator.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define MEM_ARENA_MIN_BLOCK_ALIGN DEFAULT_ALIGN

void* mem_allocate(void* allocator, size_t size, size_t align, bool clear)
{
    if (allocator) {
        return ((Allocator*)allocator)->allocate(allocator, size, align, clear);
    }

    if (clear) {
        return calloc(1, size);
    }

    return malloc(size);
}

void* mem_reallocate(void* allocator, void* ptr, size_t old_size, size_t size, size_t align)
{
    if (allocator) {
        return ((Allocator*)allocator)->reallocate(allocator, ptr, old_size, size, align);
    }

    return realloc(ptr, size);
}

typedef struct MemArenaBlockFooter {
    unsigned char* pbuffer;
    unsigned char* pend;
} MemArenaBlockFooter;

static void* mem_arena_alloc_block(MemArena* arena, size_t size, size_t align)
{
    // Adds a new block of memory to the arena.
    // The new block will contain a pointer to the old block in its footer (for cleanup).
    // The block size doubles every time.

    size_t block_size = arena->block_size;
    if (block_size < size) {
        block_size = size;
    }
    block_size *= 2;

    size_t block_align = MEM_ARENA_MIN_BLOCK_ALIGN;
    if (block_align < align) {
        block_align = align;
    }

    unsigned char* block = mem_allocate(arena->allocator, (block_size + sizeof(MemArenaBlockFooter)), block_align, false);
    if (!block) {
        return NULL;
    }

    unsigned char* pbuffer = arena->buffer;
    unsigned char* pend = arena->end;

    arena->buffer = block;
    arena->end = block + block_size;
    arena->at = block + size;
    arena->block_size = block_size;

    MemArenaBlockFooter* footer = (MemArenaBlockFooter*)arena->end;
    footer->pbuffer = pbuffer;
    footer->pend = pend;

    return (void*)block;
}

static void* mem_arena_allocate(void* allocator, size_t size, size_t align, bool clear)
{
    assert(align > 0);
    assert((align & (align - 1)) == 0);

    MemArena* arena = (MemArena*) allocator;

    void* memory = NULL;

    uintptr_t aligned_at = (((uintptr_t)arena->at) + align - 1) & ~(align - 1);
    uintptr_t new_at = aligned_at + size;

    // Allocate a new memory block if need more memory.
    if (new_at > (uintptr_t)arena->end) {
        return mem_arena_alloc_block(arena, size, align);
    }

    memory = (void*) aligned_at;
    arena->at = (unsigned char*) new_at;

    if (clear) {
        memset(memory, 0, size);
    }

    return memory;
}

static void* mem_arena_reallocate(void* allocator, void* ptr, size_t old_size, size_t size, size_t align)
{
    // TODO: If this allocation is at the top of the arena stack, allocation is not necessary!
    void* memory = mem_arena_allocate(allocator, size, align, false);
    
    if (memory && ptr) {
        memcpy(memory, ptr, old_size);
    }

    return memory;
}

static void mem_arena_free(void* allocator, void* ptr)
{
    (void)allocator;
    (void)ptr;

    // Do nothing.
}

MemArena mem_arena(Allocator* allocator, size_t block_size)
{
    MemArena arena = {0};
    arena.allocator = allocator;
    arena.block_size = block_size;
    arena.base.allocate = mem_arena_allocate; 
    arena.base.reallocate = mem_arena_reallocate;
    arena.base.free = mem_arena_free;

    return arena;
}

void mem_arena_reset(MemArena* arena)
{
    assert(arena->buffer);

    // Free all blocks, except for the last (original), which is "cleared".
    MemArenaBlockFooter* footer = (MemArenaBlockFooter*) arena->end;
    
    while (footer->pbuffer) {
        unsigned char* pbuffer = footer->pbuffer;
        unsigned char* pend = footer->pend;

        free(arena->buffer);

        arena->buffer = pbuffer;
        arena->end = pend;
        footer = (MemArenaBlockFooter*) arena->end;
    }

    arena->at = arena->buffer;
}

void mem_arena_destroy(MemArena* arena)
{
    assert(arena->buffer);

    unsigned char* buffer = arena->buffer;
    unsigned char* end = arena->end;

    while (buffer) {
        MemArenaBlockFooter footer = *(MemArenaBlockFooter*)(end);

        free(buffer);

        buffer = footer.pbuffer;
        end = footer.pend;
    }

    arena->buffer = NULL;
    arena->at = NULL;
    arena->end = NULL;
}

MemArenaState mem_arena_snapshot(MemArena* arena)
{
    MemArenaState state = {0};
    state.arena = arena;
    state.buffer = arena->buffer;
    state.at = arena->at;
    
    return state;
}

void mem_arena_restore(MemArenaState state)
{
    MemArena* arena = state.arena;
    unsigned char* dest_buffer = state.buffer;
    unsigned char* buffer = arena->buffer;
    unsigned char* end = arena->end;

    while (buffer != dest_buffer) {
        MemArenaBlockFooter footer = *(MemArenaBlockFooter*)(end);

        free(buffer);

        buffer = footer.pbuffer;
        end = footer.pend;
    }

    assert(buffer == dest_buffer);

    arena->buffer = buffer;
    arena->end = end;
    arena->at = state.at;
}

