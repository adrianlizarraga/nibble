#include "hash_map.h"

#include <assert.h>
#include <stdbool.h>

#define HASH_MAP_MIN_CAP 16

static bool hash_map_expand(HashMap* map)
{
    size_t cap = map->cap * 2;
    if (cap < HASH_MAP_MIN_CAP) {
        cap = HASH_MAP_MIN_CAP;
    }

    HashMapKey* keys = new_array(map->allocator, HashMapKey, cap, true);
    if (!keys) {
        return false;
    }

    HashMapValues* values = new_array(map->allocator, HashMapValue, cap, false);
    if (!values) {
        mem_free(map->allocator, keys);
        return false;
    }

    HashMapKey* old_keys = map->keys;
    HashMapValue* old_values = map->values;
    size_t old_cap = map->cap;

    map->keys = keys;
    map->values = values;
    map->cap = cap;
    map->len = 0;

    for (size_t i = 0; i < old_cap; ++i) {
        HashMapKey key = *(old_keys + i);
        
        if (key > 0) {
            HashMapValue value = *(old_values + i);

            hash_map_put(map, key, value);
        }
    }

    mem_free(map->allocator, old_keys);
    mem_free(map->allocator, old_values);

    return true;
}

HashMap hash_map(size_t cap_log2, Allocator* allocator)
{
    HashMap map = {0};
    map.cap = 1 << cap_log2;
    map.allocator = allocator;

    return map;
}

