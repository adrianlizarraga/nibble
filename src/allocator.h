#ifndef NIBBLE_ALLOCATOR_H
#define NIBBLE_ALLOCATOR_H
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DEFAULT_ALIGN alignof(long double)

typedef struct Allocator {
    size_t id;
    size_t num_expanded;
    char* buffer;
    char* end;
    char* at;
    char* pat;
} Allocator;

typedef struct AllocatorState {
    Allocator* allocator;
    char* buffer;
    char* at;
    char* pat;
} AllocatorState;

#define alloc_type(allocator, type, clear) (type*)mem_allocate((allocator), sizeof(type), alignof(type), (clear))
#define alloc_array(allocator, elem_type, len, clear) \
    (elem_type*)mem_allocate((allocator), sizeof(elem_type) * (len), alignof(elem_type), (clear))
#define mem_dup_array(allocator, elem_type, src, len) \
    (elem_type*)mem_dup((allocator), src, sizeof(elem_type) * (len), alignof(elem_type))

Allocator allocator_create(size_t init_size);
void allocator_reset(Allocator* allocator);
void allocator_destroy(Allocator* allocator);

void* mem_allocate(Allocator* allocator, size_t size, size_t align, bool clear);
void* mem_dup(Allocator* allocator, const void* src, size_t size, size_t align);
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

#ifdef NIBBLE_PRINT_MEM_USAGE
extern volatile uint32_t nib_alloc_count;
extern volatile uint32_t nib_free_count;
extern volatile size_t nib_alloc_size;
#endif
#endif
