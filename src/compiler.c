#include "compiler.h"

void error_stream_init(ErrorStream* stream, Allocator* allocator)
{
    memset(stream, 0, sizeof(ErrorStream));
    stream->allocator = allocator;
}

void error_stream_free(ErrorStream* stream)
{
    Error* chunk = stream->first;

    while (chunk) {
        Error* next = chunk->next;
        mem_free(stream->allocator, chunk);
        chunk = next;
    }

    stream->first = stream->last = NULL;
}

void error_stream_add(ErrorStream* stream, ProgRange range, const char* msg, size_t size)
{
    if (stream) {
        size_t chunk_size = offsetof(Error, buf) + size;
        Error* chunk = mem_allocate(stream->allocator, chunk_size, DEFAULT_ALIGN, false);

        if (chunk) {
            memcpy(chunk->msg, msg, size);
            chunk->size = size;
            chunk->next = NULL;
            chunk->range = range;

            if (!stream->first)
                stream->last = stream->first = chunk;
            else
                stream->last = stream->last->next = chunk;

            stream->count += 1;
        }
    }
}

