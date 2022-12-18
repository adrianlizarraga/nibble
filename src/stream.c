#include <string.h>
#include <assert.h>

#include "nibble.h"
#include "stream.h"


static Bucket* bucket_list_add_bucket(BucketList* bucket_list)
{
    size_t alloc_size = offsetof(Bucket, elems) + (bucket_list->bucket_cap * sizeof(void*));
    Bucket* bucket = mem_allocate(bucket_list->arena, alloc_size, DEFAULT_ALIGN, false);

    bucket->count = 0;
    bucket->next = NULL;

    if (bucket_list->first)
        bucket_list->last->next = bucket;
    else
        bucket_list->first = bucket;

    bucket_list->last = bucket;

    return bucket;
}

// NOTE: Does not assume that any bucket is full.
// However, it does assume that any elements in a bucket are contiguous.
void** bucket_list_get_elem(const BucketList* bucket_list, size_t index)
{
    if (index >= bucket_list->num_elems)
        return NULL;

    Bucket* bucket = bucket_list->first;

    while (bucket) {
        if (index < bucket->count)
            return bucket->elems + index;

        index -= bucket->count;
        bucket = bucket->next;
    }

    return NULL;
}

// NOTE: Assumes that all buckets, except the last, are full.
void** bucket_list_get_elem_packed(const BucketList* bucket_list, size_t index)
{
    if (index >= bucket_list->num_elems)
        return NULL;

    size_t elems_per_bucket = bucket_list->bucket_cap;
    size_t bucket_index = index / elems_per_bucket;
    Bucket* bucket = bucket_list->first;

    for (u64 i = 0; i < bucket_index; i += 1) {
        index -= elems_per_bucket;
        bucket = bucket->next;
    }

    assert(index < bucket->count);

    return bucket->elems + index;
}

void** bucket_list_get_last_packed(const BucketList* bucket_list)
{
    if (!bucket_list->num_elems) {
        return NULL;
    }


    Bucket* bucket = bucket_list->last;
    return bucket->elems + (bucket->count - 1);
}

void** bucket_list_add_elem(BucketList* bucket_list, void* elem)
{
    Bucket* bucket = bucket_list->last;

    if (bucket->count == bucket_list->bucket_cap)
        bucket = bucket_list_add_bucket(bucket_list);

    void** bucket_elem = bucket->elems + bucket->count;

    *bucket_elem = elem;
    bucket->count += 1;
    bucket_list->num_elems += 1;

    return bucket_elem;
}

void** bucket_list_add_elem_dup(BucketList* bucket_list, Allocator* arena, const void* elem, size_t size, size_t align)
{
    Bucket* bucket = bucket_list->last;

    if (bucket->count == bucket_list->bucket_cap)
        bucket = bucket_list_add_bucket(bucket_list);

    void** bucket_elem = bucket->elems + bucket->count;

    if (elem)
        *bucket_elem = mem_dup(arena, elem, size, align);
    else
        *bucket_elem = NULL;

    bucket->count += 1;
    bucket_list->num_elems += 1;

    return bucket_elem;
}

BucketList* new_bucket_list(Allocator* arena, size_t bucket_cap)
{
    BucketList* bucket_list = alloc_type(arena, BucketList, false);

    bucket_list_init(bucket_list, arena, bucket_cap);

    return bucket_list;
}

void bucket_list_init(BucketList* bucket_list, Allocator* arena, size_t bucket_cap)
{
    bucket_list->bucket_cap = bucket_cap;
    bucket_list->num_elems = 0;
    bucket_list->arena = arena;
    bucket_list->first = NULL;
    bucket_list->last = NULL;

    bucket_list_add_bucket(bucket_list);
}
