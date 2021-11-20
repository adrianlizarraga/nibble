#include "compiler.h"

void error_stream_init(ErrorStream* stream, Allocator* allocator)
{
    memset(stream, 0, sizeof(ErrorStream));
    stream->allocator = allocator;
}

void error_stream_free(ErrorStream* stream)
{
    Error* err = stream->first;

    while (err) {
        Error* next = err->next;
        mem_free(stream->allocator, err);
        err = next;
    }

    stream->first = stream->last = NULL;
}

void error_stream_add(ErrorStream* stream, ProgRange range, const char* msg, size_t size)
{
    if (stream) {
        size_t err_size = offsetof(Error, buf) + size;
        Error* err = mem_allocate(stream->allocator, err_size, DEFAULT_ALIGN, false);

        if (err) {
            memcpy(err->msg, msg, size);
            err->size = size;
            err->next = NULL;
            err->range = range;

            if (!stream->first)
                stream->last = stream->first = err;
            else
                stream->last = stream->last->next = err;

            stream->count += 1;
        }
    }
}

