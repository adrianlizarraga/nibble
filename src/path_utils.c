#include <unistd.h>

#include "path_utils.h"
#include "cstring.h"
#include "nibble.h"

#define ASSERT_PATH_INIT(p) assert((p)->str && array_cap((p)->str))
#define MAX_PATH_COMPS 64

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

Path path_norm(Allocator* allctr, const char* path, u32 len)
{
    assert(path[len] == '\0'); // Parsing relies on the input being null-terminated.

    Path result = {.str = array_create(allctr, char, len + 1)}; // Will never be larger than len.

    char* out = result.str;
    const char* in = path;
    bool is_abs = path[0] == NIBBLE_PATH_SEP;

    // Tracks the start of each component so that we can backtrack when the input path has a '..' component.
    char* out_comps[MAX_PATH_COMPS];
    int num_comps = 0;

    if (is_abs) {
        *out++ = NIBBLE_PATH_SEP;
    }

    out_comps[num_comps++] = out;

    while (*in) {

        // Skip consecutive '/'
        while (*in == NIBBLE_PATH_SEP) {
            in += 1;
        }

        // Check for './' component. Just skip it.
        if (in[0] == '.' && in[1] == NIBBLE_PATH_SEP) {
            in += 2;
            continue;
        }

        // Handle '../' component. Backtrack the output if necessary.
        if (in[0] == '.' && in[1] == '.' && in[2] == NIBBLE_PATH_SEP) {
            in += 3;

            // Have more than one component, so just backtrack.
            if (num_comps > 1) {
                out = out_comps[num_comps - 1];
                num_comps -= 1;
            }
            // Have one component.
            else {
                assert(num_comps == 1);

                // This is an absolute path, so '/..' is just '/'.
                if (is_abs) {
                    out = out_comps[0];
                }
                // Normalized path must start with '../'
                else {
                    out[0] = '.';
                    out[1] = '.';
                    out[2] = NIBBLE_PATH_SEP;
                    out += 3;
                }
            }

            continue;
        }

        assert(num_comps < MAX_PATH_COMPS);

        // Mark the beginning of a new component and copy from in to result.
        out_comps[num_comps++] = out;

        while (*in && *in != NIBBLE_PATH_SEP) {
            *out++ = *in++;
        }

        if (*in == NIBBLE_PATH_SEP) {
            *out++ = NIBBLE_PATH_SEP;
            in += 1;
        }
    }

    // Empty path should result in '.'
    if (out == result.str) {
        *out++ = '.';
    }
    else if (out[-1] == NIBBLE_PATH_SEP) {
        out -= 1; // Remove ending path separator.
    }

    // Write null-terminator.
    *out++ = '\0';

    // Set the length now that we now it.
    u32 result_len = out - result.str;

    assert(result_len <= len + 1);
    array_set_len(result.str, result_len);

    return result;
}

void path_free(Path* path)
{
    array_free(path->str);
}

Path path_str_join(Allocator* allctr, const char* a, u32 a_len, const char* b, u32 b_len)
{
    if (b_len == 0) {
        return path_create(allctr, a, a_len);
    }

    if (a_len == 0 || path_str_isabs(b)) {
        return path_create(allctr, b, b_len);
    }

    Path r = path_create(allctr, a, a_len); // Init r with a
    array_reserve(r.str, a_len + b_len + 2); // Will never be bigger than <a>/<b><NULL-TERM>

    const char* s = b;
    char* d = r.str + path_len(&r);

    // Add '/' between paths if necessary.
    if (a[a_len - 1] != NIBBLE_PATH_SEP) {
        *d++ = NIBBLE_PATH_SEP;
    }

    // Copy second path into result.
    while (*s) {
        *d++ = *s++;
    }

    // Write null-terminator.
    *d++ = '\0';

    // Set correct length.
    array_set_len(r.str, d - r.str);

    return r;
}

Path path_join(Allocator* allctr, const Path* a, const Path* b)
{
    ASSERT_PATH_INIT(a);
    ASSERT_PATH_INIT(b);

    return path_str_join(allctr, a->str, path_len(a), b->str, path_len(b));
}

const char* path_basename_ptr(const Path* path)
{
    ASSERT_PATH_INIT(path);
    u32 len = path_len(path);

    for (const char* p = path->str + len; p != path->str; p -= 1) {
        if (p[-1] == OS_PATH_SEP) {
            return p;
        }
    }

    return path->str;
}

const char* path_ext_ptr(const Path* path)
{
    ASSERT_PATH_INIT(path);
    u32 len = path_len(path);

    for (char* p = path->str + len; p != path->str; p -= 1) {
        if (p[-1] == '.') {
            return p;
        }
    }

    return path->str;
}

Path path_dirname(Allocator* allctr, const Path* path)
{
    ASSERT_PATH_INIT(path);

    const char* basename_ptr = path_basename_ptr(path);

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

PathRelativity path_relativity(const Path* path)
{
    u32 len = path_len(path);

    if (len == 0) {
        return PATH_REL_INVALID;
    }

    if (path_isabs(path)) {
        return PATH_REL_ABS;
    }

    if (len >= 2) {
        char c0 = path->str[0];
        char c1 = path->str[1];

        // ./ is relative the the current directory.
        if ((c0 == '.') && (c1 == NIBBLE_PATH_SEP)) {
            return PATH_REL_CURR;
        }

        // $/ is relative the main's parent directory.
        if ((c0 == '$') && (c1 == NIBBLE_PATH_SEP)) {
            return PATH_REL_PROG_ENTRY;
        }

        // ../ is relative to the current directory's parent directory.
        if ((len >= 3) && (c0 == '.' && c1 == '.' && path->str[2] == NIBBLE_PATH_SEP)) {
            return PATH_REL_PARENT;
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

Path path_abs(Allocator* allctr, const Path* cwd, const Path* path)
{
    if (path_isabs(path)) {
        return path_norm(allctr, path->str, path_len(path));
    }

    assert(path_isabs(cwd));

    Path joined = path_join(allctr, cwd, path);

    return path_norm(allctr, joined.str, path_len(&joined));
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

///////////////////////////////////////////
// Utils for working with OS and Canonical
// paths
///////////////////////////////////////////

const char nib_ext[] = "nib";
const char exe_ext[] = "exe";
const char dot_exe_ext[] = ".exe";

NibblePathErr get_import_ospath(Path* import_ospath, const StrLit* import_path_str,
                                const Path* importer_ospath, Allocator* alloc)
{
    assert(path_isabs(importer_ospath));

    Path imp = {0};
    bool starts_root = import_path_str->str[0] == NIBBLE_PATH_SEP; // Is absolute path

    if (starts_root) {
        path_init(&imp, alloc, import_path_str->str, import_path_str->len);
    }
    else {
        const char* dir_begp = importer_ospath->str;
        const char* dir_endp = path_basename_ptr(importer_ospath) - 1;

        imp = path_str_join(alloc, dir_begp, dir_endp - dir_begp, import_path_str->str, import_path_str->len);
    }

    assert(path_isabs(&imp));

    *import_ospath = path_norm(alloc, imp.str, path_len(&imp));

    path_free(&imp);

    // Check if file's path exists somewhere.
    if (path_kind(import_ospath) != FILE_REG) {
        return NIB_PATH_INV_PATH;
    }

    // Check for .nib extension.
    if (cstr_cmp(path_ext_ptr(import_ospath), nib_ext) != 0) {
        return NIB_PATH_INV_EXT;
    }

    return NIB_PATH_OK;
}

