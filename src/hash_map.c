#include "hash_map.h"

#include <assert.h>
#include <stdbool.h>

#define HASH_MAP_MIN_CAP 16
#define HASH_MAP_NULL_KEY 0

// Pelle Evensen's "Moremur" 64-bit mixer based on Murmur3 mixer
uint64_t hash_uint64(uint64_t h)
{
    h ^= h >> 27;
    h *= 0x3C79AC492BA7B653ULL;
    h ^= h >> 33;
    h *= 0x1C69B3F74AC4AE35ULL;
    h ^= h >> 27;

    return h;
}

static bool hash_map_expand(HashMap* map, size_t cap)
{
    if (cap < HASH_MAP_MIN_CAP) {
        cap = HASH_MAP_MIN_CAP;
    }

    if (cap <= map->cap) {
        return true;
    }

    HashMapEntry* old_entries = map->entries;
    HashMapEntry* entries = new_array(map->allocator, HashMapEntry, cap, true);
    if (!entries) {
        return false;
    }

    size_t old_cap = map->cap;

    map->entries = entries;
    map->cap = cap;
    map->len = 0;
    map->mask = cap - 1;

    for (size_t i = 0; i < old_cap; ++i) {
        HashMapEntry* entry = old_entries + i;

        if (entry->key > 0) {
            hash_map_put(map, entry->key, entry->value);
        }
    }

    mem_free(map->allocator, old_entries);

    return true;
}

HashMap hash_map(unsigned int cap_log2, Allocator* allocator)
{
    HashMap map = {0};
    map.allocator = allocator;

    hash_map_expand(&map, 1 << cap_log2);

    return map;
}

void hash_map_destroy(HashMap* map)
{
    mem_free(map->allocator, map->entries);
    memset(&map, 0, sizeof(HashMap));
}

uint64_t* hash_map_put(HashMap* map, uint64_t key, uint64_t value)
{
    if (key == HASH_MAP_NULL_KEY) {
        HashMapEntry* null_key = &map->null_key;

        null_key->key = 1;
        null_key->value = value;
        return &null_key->value;
    }

    // Expand at 70%
    if (10 * (map->len + 1) >= 7 * map->cap) {
        if (!hash_map_expand(map, map->cap * 2)) {
            return NULL;
        }
    }

    assert(map->len < map->cap);

    HashMapEntry* entries = map->entries;
    uint64_t i = hash_uint64(key);

    for (;;) {
        i &= map->mask;

        HashMapEntry* entry = entries + i;

        if (entry->key == HASH_MAP_NULL_KEY) {
            entry->key = key;
            entry->value = value;
            map->len += 1;
            return &entry->value;
        } else if (entry->key == key) {
            entry->value = value;
            return &entry->value;
        }

        i += 1;
    }

    return NULL;
}

uint64_t* hash_map_get(HashMap* map, uint64_t key)
{
    if (key == HASH_MAP_NULL_KEY) {
        HashMapEntry* null_key = &map->null_key;

        return null_key->key ? &null_key->value : NULL;
    }

    HashMapEntry* entries = map->entries;
    uint64_t i = hash_uint64(key) & map->mask;

    if (entries[i].key == key) {
        return &entries[i].value;
    }

    if (entries[i].key == HASH_MAP_NULL_KEY) {
        return NULL;
    }

    do {
        i = (i + 1) & map->mask;

        if (entries[i].key == key) {
            return &entries[i].value;
        }

    } while (entries[i].key);

    return NULL;
}
