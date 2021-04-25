#ifndef NIBBLE_ALLOCATOR_H
#define NIBBLE_ALLOCATOR_H
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DEFAULT_ALIGN alignof(max_align_t)

typedef struct Allocator {
    size_t id;
    size_t block_size;
    unsigned char* buffer;
    unsigned char* end;
    unsigned char* at;
    unsigned char* pat;
} Allocator;

typedef struct AllocatorState {
    Allocator* allocator;
    unsigned char* buffer;
    unsigned char* at;
    unsigned char* pat;
} AllocatorState;

#define new_type(allocator, type, clear) (type*)mem_allocate((allocator), sizeof(type), alignof(type), (clear))
#define new_array(allocator, elem_type, len, clear)                                                                    \
    (elem_type*)mem_allocate((allocator), sizeof(elem_type) * (len), alignof(elem_type), (clear))
#define mem_dup_array(allocator, elem_type, src, len)                                                                  \
    (elem_type*)mem_dup((allocator), src, sizeof(elem_type) * (len), alignof(elem_type))

Allocator allocator_create(size_t init_size);
void allocator_reset(Allocator* allocator);
void allocator_destroy(Allocator* allocator);

void* mem_allocate(Allocator* allocator, size_t size, size_t align, bool clear);
void* mem_dup(Allocator* allocator, void* src, size_t size, size_t align);
void* mem_reallocate(Allocator* allocator, void* ptr, size_t old_size, size_t size, size_t align);
void mem_free(Allocator* allocator, void* ptr);

AllocatorState allocator_get_state(Allocator* allocator);
void allocator_restore_state(AllocatorState state);

typedef struct AllocatorStats {
    size_t num_blocks;
    size_t total_size;
    size_t used;
} AllocatorStats;

AllocatorStats allocator_stats(Allocator* allocator);
void print_allocator_stats(Allocator* allocator, const char* label);

#endif
