#include "path_utils.h"
#include "cstring.h"
#include "nibble.h"

#define ASSERT_PATH_INIT(p) assert((p)->str && array_cap((p)->str))
#define MAX_PATH_COMPS 64

Path path_norm(Allocator* allctr, const char* path, u32 len)
{
    assert(path[len] == '\0'); // Parsing relies on the input being null-terminated.

    Path result;
    result->str = array_create(allctr, char, 128);

    const char* in = path;
    bool is_abs = path[0] == '/';

    // Tracks the start of each component so that we can backtrack when the input path has a '..' component.
    int out_comps[MAX_PATH_COMPS];
    int num_comps = 0;

    if (is_abs) {
        array_push(result.str, '/');
    }

    out_comps[num_comps++] = array_len(result.str);

    while (*in) {

        // Skip consecutive '/'
        while (*in == '/') {
            in += 1;
        }

        // Check for './' component. Just skip it.
        if (in[0] == '.' && in[1] == '/') {
            in += 2;
            continue;
        }

        // Handle '../' component. Backtrack the output if necessary.
        if (in[0] == '.' && in[1] == '.' && in[2] == '/') {
            in += 3;

            // Have more than one component, so just backtrack.
            if (num_comps > 1) {
                array_set_len(result.str, out_comps[num_comps - 1]);
                num_comps -= 1;
            }
            // Have one component.
            else {
                assert(num_comps == 1);

                // This is an absolute path, so '/..' is just '/'.
                if (is_abs) {
                    assert(out_comps[0] == 1);
                    array_set_len(result.str, 1);
                }
                // Normalized path must start with '../'
                else {
                    array_push(result.str, '.');
                    array_push(result.str, '.');
                    array_push(result.str, '/');
                }
            }

            continue;
        }

        assert(num_comps < MAX_PATH_COMPS);

        // Mark the beginning of a new component and copy.
        out_comps[num_comps++] = array_len(result.str);

        while (*in && *in != '/') {
            array_push(result.str, *in);
            in += 1;
        }

        if (*in == '/') {
            array_push(result.str, '/');
            in += 1;
        }
    }

    // Removing trailing '/' and null-terminate.
    if (array_len(result.str) == 0) {
        array_push(result.str, '.');
    }
    else if (array_back(result.str) == '/') {
        array_pop(result.str);
    }

    array_push(result.str, '\0');

    return result;
}

void path_free(Path* path)
{
    array_free(path->str);
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

    dst->str[len] = '\0';
    dst->len = len;
}

void path_append(Path* dst, const char* str, size_t len)
{
    ASSERT_PATH_INIT(dst);
    if (len == 0) {
        return;
    }

    size_t new_len = dst->len + len;

    if (new_len >= dst->cap) {
        const size_t _buf_size = sizeof(dst->_buf);
        const size_t cap = new_len + 1 + (_buf_size >> 2);

        path_ensure_cap(dst, cap);
    }

    char* d = dst->str + dst->len;

    while (*str) {
        *d = *str;
        d += 1;
        str += 1;
    }

    dst->str[new_len] = '\0';
    dst->len = new_len;
}

char* path_basename(Path* path)
{
    ASSERT_PATH_INIT(path);

    for (char* p = path->str + path->len; p != path->str; p -= 1) {
        if (p[-1] == OS_PATH_SEP) {
            return p;
        }
    }

    return path->str;
}

bool path_dirname(Path* dst, Path* path, Allocator* alloc)
{
    ASSERT_PATH_INIT(path);
    path_init(dst, alloc);

    const char* basename_ptr = path_basename(path);

    if (basename_ptr == path->str) {
        dst->flags |= PATH_IS_INVALID;
        return false;
    }

    const char* dirname_beg = path->str;
    const char* dirname_end = basename_ptr - 1;

    path_set(dst, dirname_beg, (dirname_end - dirname_beg));

    return true;
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

bool path_isabs(Path* path)
{
    ASSERT_PATH_INIT(path);
    return (path->len > 0) && (path->str[0] == '/');
}

PathRelativity path_relativity(Path* path)
{
    if (path->len == 0) {
        return PATH_REL_INVALID;
    }

    if (path_isabs(path)) {
        return PATH_REL_ABS;
    }

    if (path->len >= 2) {
        char c0 = path->str[0];
        char c1 = path->str[1];

        // ./ is relative the the current directory.
        if ((c0 == '.') && (c1 == '/')) {
            return PATH_REL_CURR;
        }

        // $/ is relative the main's parent directory.
        if ((c0 == '$') && (c1 == '/')) {
            return PATH_REL_PROG_ENTRY;
        }

        // ../ is relative to the current directory's parent directory.
        if ((path->len >= 3) && (c0 == '.' && c1 == '.' && path->str[2] == '/')) {
            return PATH_REL_PARENT;
        }
    }

    return PATH_REL_UNKNOWN;
}

bool dirent_it_skip(const char* name)
{
    return (cstr_cmp(name, ".") == 0) || (cstr_cmp(name, "..") == 0);
}

FileKind path_kind(Path* path)
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

///////////////////////////////////////////
// Utils for working with OS and Canonical
// paths
///////////////////////////////////////////

const char nib_ext[] = "nib";
const char exe_ext[] = "exe";
const char dot_exe_ext[] = ".exe";

NibblePathErr get_import_ospath(Path* import_ospath, const StrLit* import_path_str,
                                Path* importer_ospath, Allocator* alloc)
{
    path_init(import_ospath, alloc);

    bool starts_root = import_path_str->str[0] == NIBBLE_PATH_SEP; // Is absolute path

    if (starts_root) {
        path_set(import_ospath, import_path_str->str, import_path_str->len);
    }
    else {
        const char* dir_begp = importer_ospath->str;
        const char* dir_endp = path_basename(importer_ospath) - 1;

        path_set(import_ospath, dir_begp, dir_endp - dir_begp);

        Path import_rel_path;
        path_init(&import_rel_path, alloc);
        path_set(&import_rel_path, import_path_str->str, import_path_str->len);

        path_join(import_ospath, &import_rel_path);
    }

    // Check if file's path exists somewhere.
    if (path_kind(import_ospath) != FILE_REG) {
        return NIB_PATH_INV_PATH;
    }

    if (!path_abs(import_ospath)) {
        return NIB_PATH_INV_PATH;
    }

    // Check for .nib extension.
    if (cstr_cmp(path_ext(import_ospath), nib_ext) != 0) {
        return NIB_PATH_INV_EXT;
    }

    return NIB_PATH_OK;
}

