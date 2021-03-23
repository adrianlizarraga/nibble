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

    while (chunk) {
        ByteStreamChunk* next = chunk->next;
        mem_free(stream->allocator, chunk);
        chunk = next;
    }

    stream->first = stream->last = NULL;
}

void add_byte_stream_chunk(ByteStream* stream, const char* buf, size_t size)
{
    if (stream) {
        size_t chunk_size = offsetof(ByteStreamChunk, buf) + size;
        ByteStreamChunk* chunk = mem_allocate(stream->allocator, chunk_size, DEFAULT_ALIGN, false);  
        
        if (chunk) {
            memcpy(chunk->buf, buf, size);
            chunk->size = size;
            chunk->next = NULL;

            if (!stream->first) {
                stream->last = stream->first = chunk;
            } else {
                stream->last = stream->last->next = chunk;
            }

            stream->num_chunks += 1;
        }
    }
}
