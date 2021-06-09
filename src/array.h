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
#define array_push_elems(a, s, n)                                                                                      \
    (_array_fit(a, array_len(a) + n), memcpy(&(a)[_array_hdr(a)->len], s, (n) * sizeof(*(a))), _array_hdr(a)->len += n)
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
#define array_set_len(a, l)                                                                                            \
    ((array_cap(a) < (size_t)(l) ? array_reserve((a), (size_t)(l)), 0 : 0), (a) ? _array_hdr(a)->len = (size_t)(l) : 0)

#define array_create(alloc, type, cap) _array_reserve(NULL, (cap), sizeof(type), DEFAULT_ALIGN, alloc)

size_t ftprint_char_array(char** dst, bool nullterm, const char* format, ...);
size_t ftprintv_char_array(char** dst, bool nullterm, const char* format, va_list vargs);

void* _array_reserve(void* array, size_t len, size_t elem_size, size_t align, Allocator* allocator);
void _array_free(void* array);
#endif
