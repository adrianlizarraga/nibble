#ifndef NIBBLE_HASH_MAP_H
#define NIBBLE_HASH_MAP_H
#include <stddef.h>
#include <stdint.h>

#include "allocator.h"

typedef struct HashMapEntry {
    uint64_t key;
    uint64_t value;
} HashMapEntry;

typedef struct HashMap {
    HashMapEntry* entries;
    size_t cap;
    size_t len;
    uint64_t mask;

    HashMapEntry null_key;
    Allocator* allocator;
} HashMap;

HashMap hash_map(size_t cap_log2, Allocator* allocator);
void hash_map_put(HashMap* map, uint64_t key, uint64_t value);

uint64_t hash_uint64(uint64_t h);

#endif
