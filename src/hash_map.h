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

typedef struct InternedStr {
    size_t len;
    struct InternedStr* next;
    char str[];
} InternedStr;

const char* str_intern(Allocator* allocator, HashMap* strmap, const char* str, size_t len);

#endif
