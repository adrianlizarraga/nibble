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

typedef struct U8_Bucket U8_Bucket;
typedef struct U8_BucketList U8_BucketList;
typedef struct U8_BucketListIter U8_BucketListIter;

struct U8_Bucket {
    U8_Bucket* next;
    size_t count;
    uint8_t elems[];
};

struct U8_BucketList {
    U8_Bucket* first;
    U8_Bucket* last;
    uint32_t bucket_cap;
    uint32_t num_elems;
    Allocator* alloc;
};

U8_BucketList* new_u8_bucket_list(Allocator* alloc, uint32_t bucket_cap);
void u8_bucket_list_init(U8_BucketList* bucket_list, Allocator* alloc, uint32_t bucket_cap);
void u8_bucket_list_push(U8_BucketList* bucket_list, uint8_t elem);
uint8_t* u8_bucket_list_get(U8_BucketList* bucket_list, uint32_t index);
const uint8_t* u8_bucket_list_get_const(const U8_BucketList* bucket_list, uint32_t index);

#endif
