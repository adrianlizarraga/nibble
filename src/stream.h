#ifndef NIBBLE_STREAM_H
#define NIBBLE_STREAM_H
#include <stddef.h>
typedef struct ByteStreamChunk ByteStreamChunk;
typedef struct ByteStream ByteStream;

struct ByteStreamChunk {
    ByteStreamChunk* next;
    size_t size;
    char buf[];
};

struct ByteStream {
    ByteStreamChunk* first;
    ByteStreamChunk* last;
    size_t count;
    Allocator* allocator;
};

ByteStream byte_stream_create(Allocator* allocator);
void add_byte_stream_chunk(ByteStream* stream, const char* buf, size_t size);

typedef struct StrBucket StrBucket;
typedef struct StrBucketList StrBucketList;

struct StrBucket {
    StrBucket* next;
    size_t len;
    char* buf[];
};

struct StrBucketList {
    StrBucket* first;
    StrBucket* last;
    size_t bucket_cap;
    size_t num_strs;
    Allocator* arena;
};

StrBucketList* new_sbl(Allocator* arena, size_t bucket_cap);
char** sbl_add(StrBucketList* buckets, const char* str, size_t len);
#endif
