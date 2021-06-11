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

static StrBucket* sbl_add_bucket(StrBucketList* buckets)
{
    size_t alloc_size = offsetof(StrBucket, buf) + (buckets->bucket_cap * sizeof(char*));
    StrBucket* bucket = mem_allocate(buckets->arena, alloc_size, DEFAULT_ALIGN, false);

    bucket->next = NULL;
    bucket->len = 0;

    if (buckets->first)
        buckets->last->next = bucket;
    else
        buckets->first = bucket;

    buckets->last = bucket;

    return bucket;
}

char** sbl_add(StrBucketList* buckets, const char* str, size_t len)
{
    assert(buckets->last);

    StrBucket* bucket = buckets->last;

    if (bucket->len == buckets->bucket_cap)
        bucket = sbl_add_bucket(buckets);

    char** bucket_elem = bucket->buf + bucket->len;

    if (str)
        *bucket_elem = mem_dup(buckets->arena, str, len + 1, DEFAULT_ALIGN);

    bucket->len += 1;
    buckets->num_strs += 1;

    return bucket_elem;
}

StrBucketList* new_sbl(Allocator* arena, size_t bucket_cap)
{
    StrBucketList* buckets = alloc_type(arena, StrBucketList, false);

    buckets->arena = arena;
    buckets->bucket_cap = bucket_cap;
    buckets->first = NULL;
    buckets->last = NULL;

    sbl_add_bucket(buckets);

    return buckets;
}
