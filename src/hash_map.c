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

// FNV-1a hash
// TODO: Replace with a better hash function ;)
uint64_t hash_bytes(const void* buf, size_t len)
{
    const uint64_t FNV_PRIME = 0x00000100000001b3ULL;
    const uint64_t FNV_INIT = 0xcbf29ce484222325ULL;

    const char* b = (const char*) buf;
    uint64_t hash = FNV_INIT;

    for (size_t i = 0; i < len; ++i) {
        hash = hash ^ b[i];
        hash = hash * FNV_PRIME;
    }

    return hash;
}

static bool hash_map_expand(HashMap* map, size_t cap)
{
    if (cap < HASH_MAP_MIN_CAP) {
        cap = HASH_MAP_MIN_CAP;
    }

    if (cap <= map->cap) {
        return true;
    }

    HashMapEntry* entries = new_array(map->allocator, HashMapEntry, cap, true);
    if (!entries) {
        return false;
    }

    HashMapEntry* old_entries = map->entries;
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
    memset(map, 0, sizeof(HashMap));
}

uint64_t* hash_map_put(HashMap* map, uint64_t key, uint64_t value)
{
    if (key == HASH_MAP_NULL_KEY) {
        HashMapEntry* null_key = &map->null_key;

        null_key->key = 1;
        null_key->value = value;
        return &null_key->value;
    }

    // Expand at 60%
    if (10 * (map->len + 1) >= 6 * map->cap) {
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
    uint64_t h = hash_uint64(key);
    uint64_t i = h & map->mask;

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

typedef struct InternedStr {
    size_t len;
    struct InternedStr* next;
    char str[];
} InternedStr;

const char* str_intern(Allocator* allocator, HashMap* strmap, const char* str, size_t len)
{
    uint64_t key = hash_bytes(str, len);
    uint64_t* pval = hash_map_get(strmap, key);
    InternedStr* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (InternedStr* it = intern; it; it = it->next) {
        if ((it->len == len) && (strcmp(it->str, str) == 0)) {
            return it->str;
        }
    }

    // If we got here, need to add this string to the intern table.
    InternedStr* new_intern = mem_allocate(allocator, offsetof(InternedStr, str) + len + 1, DEFAULT_ALIGN, false);
    if (new_intern) {
        new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
        new_intern->len = len;

        memcpy(new_intern->str, str, len);
        new_intern->str[len] = '\0';

        hash_map_put(strmap, key, (uintptr_t)new_intern);
    } else {
        // TODO: Handle in a better way.
        fprintf(stderr, "[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
    }

    return new_intern->str;
}
