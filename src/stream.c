#include "stream.h"

#include <string.h>

ByteStream byte_stream_create(Allocator* allocator)
{
    ByteStream stream = {0};
    stream.allocator = allocator;

    return stream;
}

void byte_stream_destroy(ByteStream* stream)
{
    ByteStreamChunk* chunk = stream->first;

    while (chunk)
    {
        ByteStreamChunk* next = chunk->next;
        mem_free(stream->allocator, chunk);
        chunk = next;
    }

    stream->first = stream->last = NULL;
}

void add_byte_stream_chunk(ByteStream* stream, const char* buf, size_t size)
{
    if (stream)
    {
        size_t chunk_size = offsetof(ByteStreamChunk, buf) + size;
        ByteStreamChunk* chunk = mem_allocate(stream->allocator, chunk_size, DEFAULT_ALIGN, false);

        if (chunk)
        {
            memcpy(chunk->buf, buf, size);
            chunk->size = size;
            chunk->next = NULL;

            if (!stream->first)
                stream->last = stream->first = chunk;
            else
                stream->last = stream->last->next = chunk;

            stream->count += 1;
        }
    }
}

///////////////////////////////////////////////////////////////

static Bucket* bucket_list_add_bucket(BucketList* bucket_list)
{
    size_t alloc_size = offsetof(Bucket, elems) + (bucket_list->bucket_cap * sizeof(void*));
    Bucket* bucket = mem_allocate(bucket_list->arena, alloc_size, DEFAULT_ALIGN, false);

    bucket->count = 0;

    list_add_last(&bucket_list->buckets, &bucket->lnode);

    return bucket;
}

// NOTE: Does not assume that any bucket is full.
// However, it does assume that any elements in a bucket are contiguous.
void** bucket_list_get_elem(BucketList* bucket_list, size_t index)
{
    if (index >= bucket_list->num_elems)
        return NULL;

    List* head = &bucket_list->buckets;
    List* it = head->next;

    while (it != head)
    {
        Bucket* bucket = list_entry(it, Bucket, lnode);

        if (index < bucket->count)
        {
            return bucket->elems + index;
        }

        index -= bucket->count;
        it = it->next;
    }

    return NULL;
}

// NOTE: Assumes that all buckets, except the last, are full.
void** bucket_list_get_elem_packed(BucketList* bucket_list, size_t index)
{
    if (index >= bucket_list->num_elems)
        return NULL;

    size_t elems_per_bucket = bucket_list->bucket_cap;
    size_t bucket_index = index / elems_per_bucket;
    List* head = &bucket_list->buckets;
    List* it = head->next;

    for (u64 i = 0; i < bucket_index; i += 1)
    {
        index -= elems_per_bucket;
        it = it->next;
    }

    Bucket* bucket = list_entry(it, Bucket, lnode);

    assert(index < bucket->count);

    return bucket->elems + index;
}

void** bucket_list_add_elem(BucketList* bucket_list, void* elem)
{
    Bucket* bucket = list_entry(bucket_list->buckets.prev, Bucket, lnode);

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
    Bucket* bucket = list_entry(bucket_list->buckets.prev, Bucket, lnode);

    if (bucket->count == bucket_list->bucket_cap)
        bucket = bucket_list_add_bucket(bucket_list);

    void** bucket_elem = bucket->elems + bucket->count;

    if (elem)
        *bucket_elem = mem_dup(arena, elem, size, align);

    bucket->count += 1;
    bucket_list->num_elems += 1;

    return bucket_elem;
}

// NOTE: str should be a null-terminated string.
char** sstream_add(BucketList* sstream, Allocator* arena, const char* str, size_t len)
{
    return (char**)bucket_list_add_elem_dup(sstream, arena, str, len + 1, alignof(char));
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

    list_head_init(&bucket_list->buckets);
    bucket_list_add_bucket(bucket_list);
}
