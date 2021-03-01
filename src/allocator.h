#ifndef NIBBLE_ALLOCATOR_H
#define NIBBLE_ALLOCATOR_H
#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define DEFAULT_ALIGN alignof(max_align_t)

typedef struct MemArena {
    size_t min_block_size;
    size_t size;
    size_t index;
    unsigned char* buffer;
} MemArena;

typedef struct MemArenaState {
    MemArena* arena;
    size_t bytes_used;
    size_t index;
    unsigned char* buffer;
} MemArenaState;

void mem_arena_init(MemArena* arena, size_t min_block_size);
void mem_arena_clear(MemArena* arena);
void mem_arena_free(MemArena* arena);

#define mem_arena_alloc_type(arena, type, clear) \
    (type*)mem_arena_allocate((arena), sizeof(type), alignof(type), (clear))

#define mem_arena_alloc_array(arena, elem_type, len, clear) \
    (elem_type*)mem_arena_allocate((arena), sizeof(elem_type) * (len), alignof(elem_type), (clear))

void* mem_arena_allocate(MemArena* arena, size_t size, size_t align, bool clear);
void* mem_arena_reallocate(MemArena* arena, void* old_ptr, size_t old_size, size_t new_size,
                           size_t align, bool clear);

MemArenaState mem_arena_snapshot(MemArena* arena);
void mem_arena_restore(MemArenaState state);
#endif
