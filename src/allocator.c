#include "allocator.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define MEM_ARENA_DEFAULT_MIN_BLOCK_SIZE 4096

typedef struct MemArenaBlockFooter {
    unsigned char* prev_buffer;
    size_t prev_buffer_size;
} MemArenaBlockFooter;

static size_t mem_arena_aligned_index(MemArena* arena, size_t align)
{
    assert(align > 0);
    assert((align & (align - 1)) == 0);

    uintptr_t base = (uintptr_t) arena->buffer;
    uintptr_t ptr = (uintptr_t)(base + arena->index);
    uintptr_t aligned_ptr = (ptr + (align - 1)) & (-align);

    return (size_t)(aligned_ptr - base);
}

static bool mem_arena_add_block(MemArena* arena, size_t size)
{
    // Adds a new block of memory to the arena (keeps a pointer to the old buffer).
    size_t requested_size = size + sizeof(MemArenaBlockFooter);
    size_t block_size = arena->min_block_size > requested_size ? arena->min_block_size : requested_size;
    void* block = malloc(block_size);

    if (!block) {
        return false;
    }

    unsigned char* old_buffer = arena->buffer;
    size_t old_buffer_size = arena->size;

    arena->buffer = block;
    arena->size = block_size - sizeof(MemArenaBlockFooter);
    arena->index = 0;

    MemArenaBlockFooter* footer = (MemArenaBlockFooter*)(arena->buffer + arena->size);
    footer->prev_buffer = old_buffer;
    footer->prev_buffer_size = old_buffer_size;

    return true;
}

void mem_arena_init(MemArena* arena, size_t min_block_size)
{
    arena->min_block_size = min_block_size;
    arena->size = 0;
    arena->index = 0;
    arena->buffer = NULL;

    mem_arena_add_block(arena, min_block_size);
}

void* mem_arena_allocate(MemArena* arena, size_t size, size_t align, bool clear)
{
    assert(arena->buffer);

    void* memory = NULL;
    size_t index = mem_arena_aligned_index(arena, align); // Aligned version of arena->index

    // Allocate a new memory block if need more memory.
    if ((index + size) > arena->size) {
        if (!mem_arena_add_block(arena, size + size + align)) {
            return NULL;
        }

        index = mem_arena_aligned_index(arena, align);
    }

    memory = (void*) (arena->buffer + index);
    arena->index = index + size;

    if (clear) {
        memset(memory, 0, size);
    }

    return memory;
}

void* mem_arena_reallocate(MemArena* arena, void* ptr, size_t old_size, size_t new_size, size_t align,
                           bool clear)
{
    // TODO: If this allocation is at the top of the arena stack, allocation is not necessary!
    void* memory = mem_arena_allocate(arena, new_size, align, clear);
    
    if (memory && ptr) {
        memcpy(memory, ptr, old_size);
    }

    return memory;
}

void mem_arena_clear(MemArena* arena)
{
    assert(arena->buffer);

    // Free all blocks, except for the last (original), which is "cleared".
    MemArenaBlockFooter* footer = (MemArenaBlockFooter*) (arena->buffer + arena->size);
    
    while (footer->prev_buffer) {
        unsigned char* prev_buffer = footer->prev_buffer;
        size_t prev_buffer_size = footer->prev_buffer_size;

        free(arena->buffer);

        arena->buffer = prev_buffer;
        arena->size = prev_buffer_size;
        footer = (MemArenaBlockFooter*)(arena->buffer + arena->size);
    }

    arena->index = 0;
}

void mem_arena_free(MemArena* arena)
{
    assert(arena->buffer);

    unsigned char* buffer = arena->buffer;
    size_t size = arena->size;

    while (buffer) {
        MemArenaBlockFooter footer = *(MemArenaBlockFooter*)(buffer + size);

        free(buffer);

        buffer = footer.prev_buffer;
        size = footer.prev_buffer_size;
    }

    arena->buffer = NULL;
    arena->index = 0;
    arena->size = 0;
}

MemArenaState mem_arena_snapshot(MemArena* arena)
{
    MemArenaState state = {0};
    state.arena = arena;
    state.buffer = arena->buffer;
    state.index = arena->index;
    
    return state;
}

void mem_arena_restore(MemArenaState state)
{
    MemArena* arena = state.arena;
    unsigned char* dest_buffer = state.buffer;
    unsigned char* buffer = arena->buffer;
    size_t size = arena->size;

    while (buffer != dest_buffer) {
        MemArenaBlockFooter footer = *(MemArenaBlockFooter*)(buffer + size);

        free(buffer);

        buffer = footer.prev_buffer;
        size = footer.prev_buffer_size;
    }

    assert(buffer == dest_buffer);

    arena->buffer = buffer;
    arena->size = size;
    arena->index = state.index;
}

