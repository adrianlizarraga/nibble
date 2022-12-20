#include "array.h"
#include "cstring.h"

#include <stdarg.h>

typedef struct ArrayPrintCtx {
    char** dst;
    bool nullterm;
} ArrayPrintCtx;

static bool putc_array(void* data, char character)
{
    ArrayPrintCtx* ctx = data;

    if (character || ctx->nullterm)
        array_push(*ctx->dst, character);

    return true;
}

size_t ftprint_char_array(char** dst, bool nullterm, const char* format, ...)
{
    size_t n = 0;
    va_list vargs;
    ArrayPrintCtx ctx = {.dst = dst, .nullterm = nullterm};

    va_start(vargs, format);
    n = ftprintv(putc_array, &ctx, format, vargs);
    va_end(vargs);

    return n;
}

size_t ftprintv_char_array(char** dst, bool nullterm, const char* format, va_list vargs)
{
    size_t n = 0;
    ArrayPrintCtx ctx = {.dst = dst, .nullterm = nullterm};

    n = ftprintv(putc_array, &ctx, format, vargs);

    return n;
}

void* _array_reserve(void* array, size_t len, size_t elem_size, size_t align, Allocator* allocator)
{
    size_t cap = array_cap(array);
    if (cap && cap >= len)
        return array;

    size_t new_cap = cap * 2;

    if (new_cap < len)
        new_cap = len;

    if (new_cap < ARRAY_MIN_CAP)
        new_cap = ARRAY_MIN_CAP;

    size_t alloc_size = sizeof(ArrayHdr) + (elem_size * new_cap);
    ArrayHdr* hdr = array ? _array_hdr(array) : 0;
    ArrayHdr* new_hdr = NULL;

    if (hdr) {
        size_t old_size = sizeof(ArrayHdr) + (elem_size * cap);

        new_hdr = mem_reallocate(hdr->allocator, hdr, old_size, alloc_size, align);
    }
    else {
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

    if (hdr)
        mem_free(hdr->allocator, hdr);
}
