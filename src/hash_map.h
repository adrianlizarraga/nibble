#ifndef NIBBLE_HASH_MAP_H
#define NIBBLE_HASH_MAP_H
#include <stddef.h>
#include <stdint.h>

#include "allocator.h"

#define PTR_UINT(p) ((uintptr_t)((void*)(p)))
#define UINT_PTR(i, t) (t*)((uintptr_t)(i))
#define HASH_MAP_NULL_KEY 0

typedef struct HMapEntry {
    uint64_t key;
    uint64_t value;
} HMapEntry;

// Simple pointer/uint to pointer/uint hash table.
// - Power-of-two size
// - Open addressing
// - Linear probing
// - Expands by 2x at ~60% load
// - Key value of zero denotes an empty slot
typedef struct HMap {
    HMapEntry* entries;
    size_t cap;
    size_t len;
    size_t mask;

    Allocator* allocator;
} HMap;

#define clp2(c) calc_hmap_size(c)
size_t calc_hmap_size(size_t cap);

HMap hmap(unsigned int cap_log2, Allocator* allocator);
void hmap_clear(HMap* map);
void hmap_destroy(HMap* map);

uint64_t* hmap_put(HMap* map, uint64_t key, uint64_t value);
uint64_t* hmap_get(const HMap* map, uint64_t key);
void* hmap_get_obj(const HMap* map, uint64_t key);

#define FNV_INIT 0xcbf29ce484222325ULL

uint64_t hash_uint64(uint64_t h);
uint64_t hash_ptr(const void* ptr);
uint64_t hash_mix_uint64(uint64_t a, uint64_t b);
uint64_t hash_bytes(const void* buf, size_t len, uint64_t init);

#endif
