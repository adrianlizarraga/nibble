#ifndef NIBBLE_ARRAY_H
#define NIBBLE_ARRAY_H
#include "allocator.h"
#include <string.h>

#define ARRAY_MIN_CAP 16

typedef struct ArrayHdr {
    size_t len;
    size_t cap;
    Allocator* allocator;
    char data[];
} ArrayHdr;

#define _array_hdr(a) ((ArrayHdr*)a - 1)
#define _array_fit(a, n) (((n) > array_cap(a)) ? array_reserve(a, (n)) : 0)

#define array_cap(a) (!(a) ? 0 : _array_hdr(a)->cap)
#define array_len(a) (!(a) ? 0 : _array_hdr(a)->len)

#define array_push(a, ...) (_array_fit(a, array_len(a) + 1), (a)[_array_hdr(a)->len++] = (__VA_ARGS__))
#define array_pop(a) (_array_hdr(a)->len--, (a)[_array_hdr(a)->len])
#define array_back(a) ((a)[_array_hdr(a)->len - 1])
#define array_insert(a, i, ...)                                                                                        \
    (_array_fit(a, array_len(a) + 1), memmove(&(a)[(i) + 1], &(a)[i], sizeof(*(a)) * (_array_hdr(a)->len - (i))),      \
     (a)[i] = (__VA_ARGS__), _array_hdr(a)->len++)
#define array_remove(a, i)                                                                                             \
    (!(a) ? 0 : (memmove(&(a)[i], &(a)[(i) + 1], sizeof(*(a)) * (_array_hdr(a)->len - 1 - (i))), _array_hdr(a)->len--))
#define array_remove_swap(a, i) (!(a) ? 0 : (a[i] = a[_array_hdr(a)->len - 1], _array_hdr(a)->len--))

#define array_reserve(a, c) ((a) = _array_reserve((a), (c), sizeof(*(a)), DEFAULT_ALIGN, NULL))
#define array_clear(a) (!(a) ? 0 : (_array_hdr(a)->len = 0))
#define array_free(a) (((a) ? _array_free(a) : (void)0), (a) = NULL)

#define array_create(alloc, cap) _array_reserve(NULL, (cap), sizeof(*(a)), DEFAULT_ALIGN, alloc)

void* _array_reserve(void* array, size_t len, size_t elem_size, size_t align, void* allocator)
{
    size_t cap = array_cap(array);
    if (cap && cap >= len) {
        return array;
    }

    size_t new_cap = cap * 2;

    if (new_cap < len) {
        new_cap = len;
    }

    if (new_cap < ARRAY_MIN_CAP) {
        new_cap = ARRAY_MIN_CAP;
    }

    size_t alloc_size = sizeof(ArrayHdr) + (elem_size * new_cap);
    ArrayHdr* hdr = array ? _array_hdr(array) : 0;
    ArrayHdr* new_hdr = NULL;

    if (hdr) {
        size_t old_size = sizeof(ArrayHdr) + (elem_size * cap);

        new_hdr = mem_reallocate(hdr->allocator, hdr, old_size, alloc_size, align);
    } else {
        new_hdr = mem_allocate(allocator, alloc_size, align, false);
        new_hdr->allocator = allocator;
        new_hdr->len = 0;
    }

    new_hdr->cap = new_cap;

    return new_hdr->data;
}

void _array_free(void* array)
{
    ArrayHdr* hdr = _array_hdr(array);

    if (hdr) {
        mem_free(hdr->allocator, hdr);
    }
}

#endif
