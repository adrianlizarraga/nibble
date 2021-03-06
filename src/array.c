#include "array.h"

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

