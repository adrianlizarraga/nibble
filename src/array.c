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

bool _arrays_equal(void* array_a, size_t elem_size_a, void* array_b, size_t elem_size_b)
{
    // Due to lack of templates, we assume that the caller passed in arrays of the same type.
    // This check is an incomplete fail-safe.
    if (elem_size_a != elem_size_b) {
        return false;
    }

    if (array_a == array_b) {
        return true;
    }

    if (!array_a || !array_b) {
        return false;
    }

    ArrayHdr* hdr_a = _array_hdr(array_a);
    ArrayHdr* hdr_b = _array_hdr(array_b);

    if (hdr_a->len != hdr_b->len) {
        return false;
    }

    const size_t num_bytes = hdr_a->len * elem_size_a;
    for (size_t i = 0; i < num_bytes; i++) {
        if (hdr_a->data[i] != hdr_b->data[i]) {
            return false;
        }
    }

    return true;
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
        new_hdr->data[0] = '\0'; // Initialize first byte so that *arr does not use uninitialized memory
                                 // in case this is a char array. This shouldn't be needed, but is a precaution
                                 // for sloppy code that incorrectly assumes this is a c-string and does not check length first.
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
