#include <alloca.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

#include "path_utils.h"
#include "cstring.h"
#include "nibble.h"

#define ASSERT_PATH_INIT(p) assert((p)->str&& array_cap((p)->str))

const char* nib_ext = "nib";
const char* shared_lib_ext = "so";
const char* static_lib_ext = "a";
const char* obj_file_ext = "o";


Path path_create(Allocator* allctr, const char* path, u32 len)
{
    Path r = {.str = array_create(allctr, char, len + 1)};
    path_set(&r, path, len);

    return r;
}

Path path_createf(Allocator* allctr, const char* format, ...)
{
    char* str = array_create(allctr, char, 64);
    va_list vargs;

    va_start(vargs, format);
    ftprintv_char_array(&str, true, format, vargs);
    va_end(vargs);

    return (Path){.str = str};
}

void path_init(Path* dst, Allocator* allctr, const char* path, u32 len)
{
    dst->str = array_create(allctr, char, len + 1);
    path_set(dst, path, len);
}

void path_set(Path* dst, const char* path, u32 len)
{
    ASSERT_PATH_INIT(dst);
    array_set_len(dst->str, len + 1); // Set the length to avoid having to call array_push for every character copied.

    for (u32 i = 0; i < len; i += 1) {
        dst->str[i] = path[i];
    }

    dst->str[len] = '\0';
}

typedef struct PComp {
    char* loc;
    struct PComp* next;
} PComp;

// Modified version of gist authored by GitHub user starwing: https://gist.github.com/starwing/2761647
Path* path_norm(Path* path)
{
    const u32 len = path_len(path);

    assert(path->str[len] == '\0'); // Parsing depends on the input being null-terminated.
    array_reserve(path->str, 2); // Need enough space for '.'

    char* out = path->str;
    const char* in = path->str;
    bool is_abs = path_isabs(path);

    if (is_abs) {
        out += 1;
        in += 1;
    }

    // Linked-list tracks the start of each component so that we can backtrack when we encounter a '..'.
    // The head points to the start of the most recent component.
    PComp* comps = alloca(sizeof(PComp));
    comps->loc = out;
    comps->next = NULL;

    while (*in) {
        // Skip consecutive '/'
        while (*in == NIBBLE_PATH_SEP) {
            in += 1;
        }

        // Check for '.' component. Just skip it.
        if (in[0] == '.' && (in[1] == NIBBLE_PATH_SEP || !in[1])) {
            in += 1;
            continue;
        }

        // Handle '..' component. Backtrack the output if necessary.
        if (in[0] == '.' && in[1] == '.' && (in[2] == NIBBLE_PATH_SEP || !in[2])) {
            // Have more than one component, so just backtrack.
            if (comps->next != NULL) {
                out = comps->loc;
                comps = comps->next;
            }
            // Have one component.
            else if (!is_abs) {
                // Normalized path starts with '../'
                if (out != in) {
                    out[0] = '.';
                    out[1] = '.';
                    out[2] = '/';
                    out += 3;
                }
                else {
                    out += 2 + (in[2] != '\0');
                }
            }

            in += 2;
            continue;
        }

        assert(out <= in);

        // Mark the beginning of a new component and copy from in to out.
        PComp* new_comp = alloca(sizeof(PComp));
        new_comp->loc = out;
        new_comp->next = comps;
        comps = new_comp;

        while (*in && *in != NIBBLE_PATH_SEP) {
            *out++ = *in++;
        }

        if (*in == NIBBLE_PATH_SEP) {
            *out++ = NIBBLE_PATH_SEP;
            in += 1;
        }
    }

    u32 num_bytes = out - path->str;

    // Empty path should result in '.'
    if (num_bytes == 0) {
        *out++ = '.';
    }
    else if ((num_bytes > 1) && out[-1] == NIBBLE_PATH_SEP) {
        out -= 1; // Remove ending path separator.
    }

    // Write null-terminator.
    *out++ = '\0';

    // Set the final length now that we now it.
    num_bytes = out - path->str;

    assert(num_bytes - 1 <= len + 1);
    (void)len; // Unused in release
    array_set_len(path->str, num_bytes);

    return path;
}

void path_free(Path* path)
{
    array_free(path->str);
}

Path* path_join(Path* dst, const char* b, u32 b_len)
{
    u32 a_len = path_len(dst);

    if (b_len == 0) {
        return dst;
    }

    if (a_len == 0 || path_str_isabs(b)) {
        path_set(dst, b, b_len);
        return dst;
    }

    array_reserve(dst->str, a_len + b_len + 2); // Will never be bigger than <a>/<b><NULL-TERM>

    const char* s = b;
    char* d = dst->str + a_len;

    // Add '/' between paths if necessary.
    // NOTE: 'b' does not start with '/' from this point forward.
    if (dst->str[a_len - 1] != NIBBLE_PATH_SEP) {
        *d++ = NIBBLE_PATH_SEP;
    }

    // Copy second path into result.
    while (*s) {
        *d++ = *s++;
    }

    // Write null-terminator.
    *d = '\0';

    // Set correct length.
    array_set_len(dst->str, d - dst->str + 1);

    return dst;
}

const char* path_basename_ptr(const char* path, u32 len)
{
    for (const char* p = path + len; p != path; p -= 1) {
        if (p[-1] == OS_PATH_SEP) {
            return p;
        }
    }

    return path;
}

const char* path_ext_ptr(const char* path, u32 len)
{
    for (const char* p = path + len; p != path; p -= 1) {
        if (p[-1] == '.') {
            return p;
        }
    }

    return path;
}

Path path_dirname(Allocator* allctr, const Path* path)
{
    ASSERT_PATH_INIT(path);

    const char* basename_ptr = path_basename_ptr(PATH_AS_ARGS(path));

    if (basename_ptr == path->str) {
        return (Path){.str = array_create(allctr, char, 32)}; // Return empty path.
    }

    const char* dirname_beg = path->str;
    const char* dirname_end = basename_ptr - 1;

    return path_create(allctr, dirname_beg, dirname_end - dirname_beg);
}

bool path_str_isabs(const char* path)
{
    return path && (*path == NIBBLE_PATH_SEP);
}

bool path_isabs(const Path* path)
{
    return (path_len(path) > 0) && (path->str[0] == NIBBLE_PATH_SEP);
}

PathRelativity path_relativity(const char* path, u32 len)
{
    if (len == 0) {
        return PATH_REL_INVALID;
    }

    if (path_str_isabs(path)) {
        return PATH_REL_ROOT;
    }

    if (len >= 2) {
        char c0 = path[0];
        char c1 = path[1];

        // ./ is relative the the current directory.
        if ((c0 == '.') && (c1 == NIBBLE_PATH_SEP)) {
            return PATH_REL_CURR;
        }

        // ../ is relative to the current directory's parent directory.
        if ((len >= 3) && (c0 == '.' && c1 == '.' && path[2] == NIBBLE_PATH_SEP)) {
            return PATH_REL_CURR;
        }
        // $/ is relative the main's parent directory.
        if ((c0 == '$') && (c1 == NIBBLE_PATH_SEP)) {
            return PATH_REL_PROG_ENTRY;
        }
    }

    return PATH_REL_UNKNOWN;
}

bool dirent_it_skip(const char* name)
{
    return (cstr_cmp(name, ".") == 0) || (cstr_cmp(name, "..") == 0);
}

FileKind path_kind(const Path* path)
{
    struct stat sb;
    int ret = stat(path->str, &sb);

    if (ret == -1) {
        return FILE_NONE;
    }

    FileKind kind = FILE_NONE;

    if ((sb.st_mode & S_IFMT) == S_IFREG) {
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

Path get_curr_dir(Allocator* allctr)
{
    Path r = {.str = array_create(allctr, char, PATH_MAX)};

    char* ret = getcwd(r.str, PATH_MAX);

    if (ret != NULL) {
        array_set_len(r.str, cstr_len(r.str) + 1);
    }

    return r;
}

Path* path_abs(Path* path, const char* cwd_str, u32 cwd_len)
{
    if (path_isabs(path)) {
        return path_norm(path);
    }

    assert(path_str_isabs(cwd_str));

    // Copy 'path' into tmp array
    u32 n = path_len(path) + 1;
    char* tmp = alloca(n);
    memcpy(tmp, path->str, n);

    // Overwrite 'path' with 'cwd'.
    path_set(path, cwd_str, cwd_len);

    // Join cwd/path and normalize
    return path_norm(path_join(path, tmp, n));
}

bool path_real(Path* dst, const Path* path)
{
    ASSERT_PATH_INIT(path);
    ASSERT_PATH_INIT(dst);

    array_reserve(dst->str, PATH_MAX);

    bool success = realpath(path->str, dst->str) != NULL;
    array_set_len(dst->str, success ? cstr_len(dst->str) : 0);

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
    it->os_handle = opendir(path_str);

    if (!it->os_handle) {
        it->flags = 0;
        return;
    }

    it->flags = DIRENT_IS_VALID;

    path_init(&it->base, alloc, path_str, cstr_len(path_str));
    path_init(&it->name, alloc, NULL, 0);

    dirent_it_next(it);
}
