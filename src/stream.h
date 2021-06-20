#ifndef NIBBLE_STREAM_H
#define NIBBLE_STREAM_H
#include <stddef.h>
#include "llist.h"
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

typedef struct Bucket Bucket;
typedef struct BucketList BucketList;

struct Bucket {
    List lnode;
    size_t count;
    void* elems[];
};

struct BucketList {
    List buckets;
    size_t bucket_cap;
    size_t num_elems;
};

BucketList* new_bucket_list(Allocator* arena, size_t bucket_cap);
void bucket_list_init(BucketList* bucket_list, Allocator* arena, size_t bucket_cap);

void** bucket_list_add_elem(BucketList* bucket_list, void* elem);
void** bucket_list_add_elem_dup(BucketList* bucket_list, Allocator* arena, const void* elem, size_t size, size_t align);
char** sstream_add(BucketList* sstream, Allocator* arena, const char* str, size_t len);

void** bucket_list_get_elem(BucketList* bucket_list, size_t index);
void** bucket_list_get_elem_packed(BucketList* bucket_list, size_t index);
#endif
