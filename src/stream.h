#ifndef NIBBLE_STREAM_H
#define NIBBLE_STREAM_H
#include <stddef.h>

typedef struct ByteStreamChunk {
    struct ByteStreamChunk* next;
    size_t size;
    char buf[];
} ByteStreamChunk;

typedef struct ByteStream {
    ByteStreamChunk* first;
    ByteStreamChunk* last;
    size_t num_chunks;
    Allocator* allocator;
} ByteStream;

ByteStream byte_stream_create(Allocator* allocator);
void add_byte_stream_chunk(ByteStream* stream, const char* buf, size_t size);
#endif
