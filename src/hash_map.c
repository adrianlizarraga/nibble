#include "hash_map.h"
#include "cstring.h"

#include <assert.h>
#include <stdbool.h>
#include <string.h>

// TODO: Use clp2 from Hacker's Delight 2nd edition, pg 62.
size_t calc_hmap_size(size_t cap)
{
    size_t pow2_cap = 1;
    size_t log2_cap = 0;

    while (cap > pow2_cap) {
        pow2_cap = pow2_cap << 1;
        log2_cap += 1;
    }

    return log2_cap;
}

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

uint64_t hash_ptr(const void* ptr)
{
    return hash_uint64((uintptr_t)ptr);
}

uint64_t hash_mix_uint64(uint64_t a, uint64_t b)
{
    return hash_uint64(a ^ b);
}

// FNV-1a hash
// NOTE: Use FNV_INIT as the initial value if not chaining hash_bytes() calls.
// TODO: Replace with a better hash function ;)
uint64_t hash_bytes(const void* buf, size_t len, u64 init)
{
    const uint64_t FNV_PRIME = 0x00000100000001b3ULL;

    const char* b = (const char*)buf;
    uint64_t hash = init;

    for (size_t i = 0; i < len; ++i) {
        hash = hash ^ b[i];
        hash = hash * FNV_PRIME;
    }

    return hash;
}

static bool hmap_expand(HMap* map, size_t cap)
{
    if (cap <= map->cap)
        return true;

    HMapEntry* entries = alloc_array(map->allocator, HMapEntry, cap, true);

    if (!entries)
        return false;

    HMapEntry* old_entries = map->entries;
    size_t old_cap = map->cap;

    map->entries = entries;
    map->cap = cap;
    map->len = 0;
    map->mask = cap - 1;

    for (size_t i = 0; i < old_cap; ++i) {
        HMapEntry* entry = old_entries + i;

        if (entry->key > 0)
            hmap_put(map, entry->key, entry->value);
    }

    mem_free(map->allocator, old_entries);

    return true;
}

HMap hmap(unsigned int cap_log2, Allocator* allocator)
{
    HMap map = {0};
    map.allocator = allocator;

    hmap_expand(&map, 1 << cap_log2);

    return map;
}

void hmap_clear(HMap* map)
{
    if (map->len) {
        memset(&map->entries, 0, map->cap * sizeof(HMapEntry));
        map->len = 0;
    }
}

void hmap_destroy(HMap* map)
{
    mem_free(map->allocator, map->entries);
    memset(map, 0, sizeof(HMap));
}

uint64_t* hmap_put(HMap* map, uint64_t key, uint64_t value)
{
    assert(key != HASH_MAP_NULL_KEY);

    // Expand at 60%
    if (10 * (map->len + 1) >= 6 * map->cap) {
        if (!hmap_expand(map, map->cap * 2))
            return NULL;
    }

    HMapEntry* entries = map->entries;
    uint64_t i = hash_uint64(key);

    for (;;) {
        i &= map->mask;

        HMapEntry* entry = entries + i;

        if (entry->key == HASH_MAP_NULL_KEY) {
            entry->key = key;
            entry->value = value;
            map->len += 1;
            return &entry->value;
        }
        else if (entry->key == key) {
            entry->value = value;
            return &entry->value;
        }

        i += 1;
    }

    return NULL;
}

uint64_t* hmap_get(const HMap* map, uint64_t key)
{
    assert(key != HASH_MAP_NULL_KEY);

    if (!map->entries)
        return NULL;

    HMapEntry* entries = map->entries;
    uint64_t h = hash_uint64(key);
    uint64_t i = h & map->mask;

    if (entries[i].key == key)
        return &entries[i].value;

    if (entries[i].key == HASH_MAP_NULL_KEY)
        return NULL;

    do {
        i = (i + 1) & map->mask;

        if (entries[i].key == key)
            return &entries[i].value;
    } while (entries[i].key);

    return NULL;
}

void* hmap_get_obj(const HMap* map, uint64_t key)
{
    uint64_t* obj_ptr = hmap_get(map, key);

    return obj_ptr ? (void*)*obj_ptr : NULL;
}

