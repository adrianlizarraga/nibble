#ifndef NIBBLE_HASH_MAP_H
#define NIBBLE_HASH_MAP_H
#include <stddef.h>
#include <stdint.h>

#include "allocator.h"

typedef uintptr_t HashMapKey;
typedef uintptr_t HashMapValue;

typedef struct HashMap {
    HashMapKey* keys;
    HashMapValue* values;
    size_t cap;
    size_t len;
    Allocator* allocator;
} HashMap;

HashMap hash_map(size_t cap_log2, Allocator* allocator);

#endif
