#ifndef NIBBLE_HASH_MAP_H
#define NIBBLE_HASH_MAP_H
#include <stddef.h>
#include <stdint.h>

#include "allocator.h"

typedef struct HashMapEntry {
    uint64_t key;
    uint64_t value;
} HashMapEntry;

// Simple pointer/uint to pointer/uint hash table.
// - Power-of-two size
// - Open addressing
// - Linear probing
// - Expands by 2x at ~60% load
// - Key value of zero denotes an empty slot
typedef struct HashMap {
    HashMapEntry* entries;
    size_t cap;
    size_t len;
    size_t mask;

    HashMapEntry null_key;
    Allocator* allocator;
} HashMap;

HashMap hash_map(unsigned int cap_log2, Allocator* allocator);
void hash_map_destroy(HashMap* map);

uint64_t* hash_map_put(HashMap* map, uint64_t key, uint64_t value);
uint64_t* hash_map_get(HashMap* map, uint64_t key);

uint64_t hash_uint64(uint64_t h);
uint64_t hash_bytes(const void* buf, size_t len);

const char* str_intern(Allocator* allocator, HashMap* strmap, const char* str, size_t len);

#endif
