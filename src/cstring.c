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

#define ASSERT_PATH_INIT(p) assert((p)->str && (p)->cap)

static void path_norm(Path* path)
{
    ASSERT_PATH_INIT(path);

    if (OS_PATH_SEP != NIBBLE_PATH_SEP) {
        for (char* p = path->str; *p; p += 1) {
            if (*p == NIBBLE_PATH_SEP) {
                *p = OS_PATH_SEP;
            }
        }
    }

    // Remove ending `/` (if not at the beginning of path)
    if ((path->len > 1) && (path->str[path->len - 1] == OS_PATH_SEP)) {
        path->str[path->len - 1] = '\0';
        path->len -= 1;
    }
}

static void path_ensure_cap(Path* path, size_t cap)
{
    if (path->cap >= cap) {
        return;
    }

    char* str = alloc_array(path->alloc, char, cap, false);

    for (size_t i = 0; i <= path->len; i += 1) {
        str[i] = path->str[i];
    }

    if (path->str != path->_buf) {
        mem_free(path->alloc, path->str);
    }

    path->str = str;
    path->cap = cap;
}

void path_free(Path* path)
{
    ASSERT_PATH_INIT(path);

    if (path->str != path->_buf) {
        mem_free(path->alloc, path->str);
        path->len = 0;
        path->cap = 0;
    }
}

void path_init(Path* path, Allocator* alloc)
{
    path->str = path->_buf;
    path->len = 0;
    path->cap = sizeof(path->_buf);
    path->alloc = alloc;
    path->_buf[0] = 0;
}

void path_set(Path* path, const char* src, size_t src_len)
{
    ASSERT_PATH_INIT(path);

    if (src_len >= path->cap) {
        path_free(path);

        const size_t _buf_size = sizeof(path->_buf);
        const size_t cap = src_len + 1 + (_buf_size >> 2);

        path->str = alloc_array(path->alloc, char, cap, false);
        path->cap = cap;
    }

    // Copy src characters (including null char) into our path.
    for (size_t i = 0; i <= src_len; i += 1) {
        path->str[i] = src[i];
    }

    path->len = src_len;

    path_norm(path);
}

void path_join(Path* dst, Path* src)
{
    ASSERT_PATH_INIT(dst);
    ASSERT_PATH_INIT(src);

    if (src->len == 0) {
        return;
    }

    bool dst_ends_sep = dst->len && (dst->str[dst->len - 1] == OS_PATH_SEP);
    bool src_begs_sep = src->str[0] == OS_PATH_SEP;

    size_t len = dst->len + src->len + 1; // <dst>/<src>

    char* s = src->str;

    if (dst_ends_sep) {
        len -= 1;
    }

    if (src_begs_sep) {
        len -= 1;
        s += 1;
    }

    // Allocate a bigger destination buffer if necessary.
    if (len >= dst->cap) {
        const size_t _buf_size = sizeof(dst->_buf);
        const size_t cap = len + 1 + (_buf_size >> 2);

        path_ensure_cap(dst, cap);
    }

    char* d = dst->str + dst->len;

    if (dst_ends_sep) {
        d -= 1;
    }

    // Start appending src to dst
    *d = OS_PATH_SEP;
    d += 1;

    while (*s) {
        *d = *s;
        d += 1;
        s += 1;
    }

    dst->len = len;
}

char* path_filename(Path* path)
{
    ASSERT_PATH_INIT(path);

    for (char* p = path->str + path->len; p != path->str; p -= 1) {
        if (p[-1] == OS_PATH_SEP) {
            return p;
        }
    }

    return path->str;
}

char* path_ext(Path* path)
{
    ASSERT_PATH_INIT(path);

    for (char* p = path->str + path->len; p != path->str; p -= 1) {
        if (p[-1] == '.') {
            return p;
        }
    }

    return path->str;
}

bool dirent_it_skip(const char* name)
{
    return (cstr_cmp(name, ".") == 0) || (cstr_cmp(name, "..") == 0);
}

#ifdef _WIN32

FileKind path_kind(Path* path)
{

    DWORD attribs = GetFileAttributesA(path->str);

    if (attribs == INVALID_FILE_ATTRIBUTES) {
        return FILE_NONE;
    }

    FileKind kind = FILE_NONE;

    if (attribs & FILE_ATTRIBUTE_NORMAL) {
        kind = FILE_REG;
    }
    else if (attribs & FILE_ATTRIBUTE_DIRECTORY) {
        kind = FILE_DIR;
    }
    else {
        kind = FILE_OTHER;
    }

    return kind;
}

bool path_abs(Path* path)
{
    ASSERT_PATH_INIT(path);

    Path rel;
    path_init(&rel, path->alloc);
    path_set(&rel, path->str, path->len);

    bool success = _fullpath(path->str, rel.str, path->cap) != NULL;

    if (success) {
        path->len = cstr_len(path->str);
        path->flags |= PATH_IS_CANONICAL;
        path->flags &= ~PATH_IS_INVALID;
    }
    else {
        path->len = 0;
        path->flags |= PATH_IS_INVALID;
    }

    path_free(&rel);

    return success;
}

void dirent_it_free(DirentIter* it)
{
    if (it->flags & DIRENT_IS_VALID) {
        it->flags &= ~DIRENT_IS_VALID;
        _findclose((intptr_t)it->os_handle);
        path_free(&it->base);
        path_free(&it->name);
    }
}

void dirent_it_next(DirentIter* it)
{
    if (!(it->flags & DIRENT_IS_VALID)) {
        return;
    }

    bool is_dir = false;

    do {
        struct _finddata_t fileinfo;
        intptr_t result = _findnext((intptr_t)it->os_handle, &fileinfo);

        if (result != 0) {
            dirent_it_free(it);
            return;
        }

        path_set(&it->name, fileinfo.name, cstr_len(fileinfo.name));

        is_dir = fileinfo.attrib & _A_SUBDIR;
    } while (dirent_it_skip(it->name.str));

    if (is_dir) {
        it->flags |= DIRENT_IS_DIR;
    }
}

void dirent_it_init(DirentIter* it, const char* path_str, Allocator* alloc)
{
    path_init(&it->base, alloc);
    path_init(&it->name, alloc);

    // Create a temporary `filespec` Path that adds `/*` to the base path.
    Path filespec;
    size_t path_len = cstr_len(path_str);
    path_init(&filespec, alloc);
    path_set(&filespec, path_str, path_len);

    bool ends_sep = filespec.str[filespec.len - 1] == OS_PATH_SEP;
    size_t needed_space = ends_sep ? 1 : 2;

    path_ensure_cap(&filespec, filespec.len + 1 + needed_space);

    if (!ends_sep) {
        filespec.str[filespec.len++] = OS_PATH_SEP;
    }

    filespec.str[filespec.len++] = '*';
    filespec.str[filespec.len] = '\0';

    // Get the first directory entry.
    struct _finddata_t fileinfo;
    intptr_t handle = _findfirst(filespec.str, &fileinfo);

    it->os_handle = (void*)handle;

    if (handle == -1) {
        it->flags = 0;
        return;
    }

    it->flags = DIRENT_IS_VALID;
    if (fileinfo.attrib & _A_SUBDIR) {
        it->flags |= DIRENT_IS_DIR;
    }

    path_set(&it->base, path_str, path_len);
    path_set(&it->name, fileinfo.name, cstr_len(fileinfo.name));

    if (dirent_it_skip(it->name.str)) {
        dirent_it_next(it);
    }
}
#else

FileKind path_kind(Path* path)
{

    struct stat sb;
    int ret = stat(path->str, &sb);

    if (ret == -1) {
        return FILE_NONE;
    }

    FileKind kind = FILE_NONE;

    if ((sb.st_mode & S_IFMT) ==  S_IFREG) {
        kind = FILE_REG;
    }
    else if ((sb.st_mode & S_IFMT) == S_IFDIR) {
        kind = FILE_DIR;
    }
    else {
        kind = FILE_OTHER;
    }

    return kind;
}

bool path_abs(Path* path)
{
    ASSERT_PATH_INIT(path);

    Path rel;
    path_init(&rel, path->alloc);
    path_set(&rel, path->str, path->len);

    assert(path->cap >= PATH_MAX);

    bool success = realpath(rel.str, path->str) != NULL;

    if (success) {
        path->len = cstr_len(path->str);
        path->flags |= PATH_IS_CANONICAL;
        path->flags &= ~PATH_IS_INVALID;
    }
    else {
        path->len = 0;
        path->flags |= PATH_IS_INVALID;
    }

    path_free(&rel);

    return success;
}

void dirent_it_free(DirentIter* it)
{
    if (it->flags & DIRENT_IS_VALID) {
        it->flags &= ~DIRENT_IS_VALID;
        closedir(it->os_handle);
        path_free(&it->base);
        path_free(&it->name);
    }
}

void dirent_it_next(DirentIter* it)
{
    if (!(it->flags & DIRENT_IS_VALID)) {
        return;
    }

    bool is_dir = false;

    do {
        struct dirent* entry = readdir(it->os_handle);

        if (!entry) {
            dirent_it_free(it);
            return;
        }

        path_set(&it->name, entry->d_name, cstr_len(entry->d_name));

        is_dir = entry->d_type & DT_DIR;
    } while (dirent_it_skip(it->name.str));

    if (is_dir) {
        it->flags |= DIRENT_IS_DIR;
    }
}

void dirent_it_init(DirentIter* it, const char* path_str, Allocator* alloc)
{
    path_init(&it->base, alloc);
    path_init(&it->name, alloc);

    it->os_handle = opendir(path_str);

    if (!it->os_handle) {
        it->flags = 0;
        return;
    }

    it->flags = DIRENT_IS_VALID;

    path_set(&it->base, path_str, cstr_len(path_str));
    dirent_it_next(it);
}

#endif // _WIN32
