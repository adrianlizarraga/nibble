#ifndef NIBBLE_STREAM_H
#define NIBBLE_STREAM_H
#include <stddef.h>
#include "allocator.h"
#include "llist.h"

typedef struct Bucket Bucket;
typedef struct BucketList BucketList;

struct Bucket {
    Bucket* next;
    size_t count;
    void* elems[];
};

struct BucketList {
    Bucket* first;
    Bucket* last;
    size_t bucket_cap;
    size_t num_elems;
    Allocator* arena;
};

BucketList* new_bucket_list(Allocator* arena, size_t bucket_cap);
void bucket_list_init(BucketList* bucket_list, Allocator* arena, size_t bucket_cap);

// TODO: const-correctness. Need to remove offset field from var syms
void** bucket_list_add_elem(BucketList* bucket_list, void* elem);

void** bucket_list_add_elem_dup(BucketList* bucket_list, Allocator* arena, const void* elem, size_t size, size_t align);

void** bucket_list_get_elem(const BucketList* bucket_list, size_t index);
void** bucket_list_get_elem_packed(const BucketList* bucket_list, size_t index);
void** bucket_list_get_last_packed(const BucketList* bucket_list);
#endif
