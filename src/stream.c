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

static StrBucket* sstream_add_bucket(StrStream* sstream)
{
    size_t alloc_size = offsetof(StrBucket, buf) + (sstream->bucket_cap * sizeof(char*));
    StrBucket* bucket = mem_allocate(sstream->arena, alloc_size, DEFAULT_ALIGN, false);

    bucket->next = NULL;
    bucket->len = 0;

    if (sstream->first)
        sstream->last->next = bucket;
    else
        sstream->first = bucket;

    sstream->last = bucket;

    return bucket;
}

char** sstream_add(StrStream* sstream, const char* str, size_t len)
{
    assert(sstream->last);

    StrBucket* bucket = sstream->last;

    if (bucket->len == sstream->bucket_cap)
        bucket = sstream_add_bucket(sstream);

    char** bucket_elem = bucket->buf + bucket->len;

    if (str)
        *bucket_elem = mem_dup(sstream->arena, str, len + 1, DEFAULT_ALIGN);

    bucket->len += 1;
    sstream->num_strs += 1;

    return bucket_elem;
}

StrStream* new_sstream(Allocator* arena, size_t bucket_cap)
{
    StrStream* sstream = alloc_type(arena, StrStream, false);

    sstream->arena = arena;
    sstream->bucket_cap = bucket_cap;
    sstream->first = NULL;
    sstream->last = NULL;

    sstream_add_bucket(sstream);

    return sstream;
}
