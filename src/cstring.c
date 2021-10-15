#include "cstring.h"
#include "array.h"

int cstr_cmp(const char* str1, const char* str2)
{
    while ((*str1 == *str2) && *str1 && *str2) {
        str1 += 1;
        str2 += 1;
    }

    return *(unsigned char*)str1 - *(unsigned char*)str2;
}

int cstr_ncmp(const char* str1, const char* str2, size_t num)
{
    if (!num)
        return 0;

    num -= 1;

    while ((*str1 == *str2) && *str1 && *str2 && num) {
        str1 += 1;
        str2 += 1;
        num -= 1;
    }

    return *(unsigned char*)str1 - *(unsigned char*)str2;
}

// From "Hacker's Delight 2nd edition", pg 118. Attributed to: Mycroft, Alan. Newsgroup comp.arch, April 8, 1987.
// Returns non-zero value if the uint32_t has a zero-byte. Works for any endianness.
#define U32_HAS_ZERO_BYTE(x) (((x)-0x01010101) & (~(x)) & 0x80808080)

size_t cstr_len(const char* str)
{
    assert(str);
    const char* s = str;

#ifdef __GNUC__
    // Calc length until s pointer is aligned to a 4-byte boundary.
    const uint32_t align_mask = sizeof(uint32_t) - 1;

    while (((uintptr_t)s & align_mask)) {
        if (!*s)
            return s - str;

        s += 1;
    }

    // Iterate over the data in 4-byte increments until the null terminator is found.
    typedef uint32_t __attribute__((__may_alias__)) uword;
    const uword* w = (const void*)s;

    while (!U32_HAS_ZERO_BYTE(*w))
        w += 1;

    s = (const void*)w; // Point to the beginning of the word containing the 0 byte.
#endif

    while (*s)
        s += 1;

    return s - str;
}

void cstr_tolower(char* str)
{
    if (str) {
        while (*str) {
            // NOTE: A table-based lookup approach would be faster (probably).
            if (*str >= 'A' && *str <= 'Z') {
                *str += 'a' - 'A';
            }

            str += 1;
        }
    }
}

char* cstr_escape(Allocator* allocator, const char* str, size_t len, char extra_escape)
{
    char* result = array_create(allocator, char, len << 1); // Capacity is initialized to twice the length.

    for (size_t i = 0; i < len; i += 1) {
        char c = str[i];

        switch (c) {
        case '\0':
            ftprint_char_array(&result, false, "\\0");
            break;
        case '\a':
            ftprint_char_array(&result, false, "\\a");
            break;
        case '\b':
            ftprint_char_array(&result, false, "\\b");
            break;
        case '\f':
            ftprint_char_array(&result, false, "\\f");
            break;
        case '\n':
            ftprint_char_array(&result, false, "\\n");
            break;
        case '\r':
            ftprint_char_array(&result, false, "\\r");
            break;
        case '\t':
            ftprint_char_array(&result, false, "\\t");
            break;
        case '\v':
            ftprint_char_array(&result, false, "\\v");
            break;
        case '\\':
            ftprint_char_array(&result, false, "\\\\");
            break;
        case '\'':
            ftprint_char_array(&result, false, "\\\'");
            break;
        case '\"':
            ftprint_char_array(&result, false, "\\\"");
            break;
        case '\?':
            ftprint_char_array(&result, false, "\\?");
            break;
        default:
            if (is_cntrl(c) || (c == extra_escape)) {
                ftprint_char_array(&result, false, "\\x%.2x", c);
            }
            else {
                array_push(result, c);
            }
            break;
        }
    }

    array_push(result, '\0');

    return result;
}

// Generated with tools/char_props_printer.c
const unsigned char char_props[256] = {
    0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x9, 0x9, 0x9, 0x8, 0x9, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8,
    0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x6, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x4,
    0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
    0x4, 0x4, 0x4, 0x0, 0x0, 0x0, 0x0, 0x4, 0x0, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4,
    0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x0, 0x0, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
};

// Converts a numeric character to an integer value. Values are biased by +1
// so that a result of 0 is known to be invalid.
const unsigned char char_to_biased_digit[256] = {
    ['0'] = 1,  ['1'] = 2,  ['2'] = 3,  ['3'] = 4,  ['4'] = 5,  ['5'] = 6,  ['6'] = 7,  ['7'] = 8,
    ['8'] = 9,  ['9'] = 10, ['a'] = 11, ['b'] = 12, ['c'] = 13, ['d'] = 14, ['e'] = 15, ['f'] = 16,
    ['A'] = 11, ['B'] = 12, ['C'] = 13, ['D'] = 14, ['E'] = 15, ['F'] = 16,
};

const char escaped_to_char[256] = {
    ['0'] = '\0', ['a'] = '\a', ['b'] = '\b',  ['f'] = '\f',  ['n'] = '\n', ['r'] = '\r',
    ['t'] = '\t', ['v'] = '\v', ['\\'] = '\\', ['\''] = '\'', ['"'] = '"',  ['?'] = '?',
};

void u32_set_bit(u32* mask, u8 bit)
{
    *mask |= (1 << bit);
}

void u32_unset_bit(u32* mask, u8 bit)
{
    *mask &= ~(1 << bit);
}

bool u32_is_bit_set(u32 mask, u8 bit)
{
    return mask & (1 << bit);
}
