#ifndef NIBBLE_ALLOCATOR_H
#define NIBBLE_ALLOCATOR_H
#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define DEFAULT_ALIGN alignof(max_align_t)

typedef void* MemAllocFunc(void* allocator, size_t size, size_t align, bool clear);
typedef void* MemReallocFunc(void* allocator, void* ptr, size_t old_size, size_t size, size_t align);
typedef void MemFreeFunc(void* allocator, void* ptr);

typedef struct Allocator {
    MemAllocFunc* allocate;
    MemReallocFunc* reallocate;
    MemFreeFunc* free;
} Allocator;

#define new_type(allocator, type, clear) \
    (type*)mem_allocate((allocator), sizeof(type), alignof(type), (clear))

#define new_array(allocator, elem_type, len, clear) \
    (elem_type*)mem_allocate((allocator), sizeof(elem_type) * (len), alignof(elem_type), (clear))

void* mem_allocate(void* allocator, size_t size, size_t align, bool clear);
void* mem_reallocate(void* allocator, void* ptr, size_t old_size, size_t size, size_t align);
void mem_free(void* allocator, void* ptr);

typedef struct MemArena {
    Allocator base;
    Allocator* allocator;
    size_t block_size;
    unsigned char* buffer;
    unsigned char* end;
    unsigned char* at;
} MemArena;

typedef struct MemArenaState {
    MemArena* arena;
    unsigned char* buffer;
    unsigned char* at;
} MemArenaState;

MemArena mem_arena(Allocator* allocator, size_t block_size);
void mem_arena_reset(MemArena* arena);
void mem_arena_destroy(MemArena* arena);

MemArenaState mem_arena_snapshot(MemArena* arena);
void mem_arena_restore(MemArenaState state);


#endif
