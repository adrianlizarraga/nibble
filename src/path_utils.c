#include "path_utils.h"
#include "cstring.h"
#include "nibble.h"

#define ASSERT_PATH_INIT(p) assert((p)->str && (p)->cap)

void path_norm(Path* path, char old_sep, char new_sep)
{
    ASSERT_PATH_INIT(path);

    if (new_sep != old_sep) {
        for (char* p = path->str; *p; p += 1) {
            if (*p == old_sep) {
                *p = new_sep;
            }
        }
    }

    // Remove ending `/` (if not at the beginning of path)
    if ((path->len > 1) && (path->str[path->len - 1] == new_sep)) {
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

    for (size_t i = 0; i < path->len; i += 1) {
        str[i] = path->str[i];
    }

    str[path->len] = '\0';

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
    size_t i = 0;

    while (i < src_len) {
        path->str[i] = src[i];
        i += 1;
    }

    path->str[i] = '\0';
    path->len = src_len;

    path_norm(path, NIBBLE_PATH_SEP, OS_PATH_SEP);
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

void cpath_str_to_ospath(Allocator* alloc, Path* dst, const char* cpath_str, size_t cpath_len, const Path* base_ospath)
{
    Path cpath;
    path_init(&cpath, alloc);
    path_set(&cpath, cpath_str, cpath_len);

    path_init(dst, alloc);
    path_set(dst, base_ospath->str, base_ospath->len);
    path_join(dst, &cpath);
}

NibblePathErr get_import_ospath(Path* import_ospath, const StrLit* import_path_str, const Path* base_ospath,
                                Path* importer_ospath, Allocator* alloc)
{
    path_init(import_ospath, alloc);

    Path import_rel_path;
    path_init(&import_rel_path, alloc);
    path_set(&import_rel_path, import_path_str->str, import_path_str->len);

    bool starts_root = import_path_str->str[0] == NIBBLE_PATH_SEP;

    if (starts_root) {
        path_set(import_ospath, base_ospath->str, base_ospath->len);
    }
    else {
        const char* dir_begp = importer_ospath->str;
        const char* dir_endp = path_filename(importer_ospath) - 1;

        path_set(import_ospath, dir_begp, dir_endp - dir_begp);
    }

    path_join(import_ospath, &import_rel_path);

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

NibblePathErr ospath_to_cpath(Path* dst_path, const Path* src_ospath, const Path* base_ospath, Allocator* alloc)
{
    // TODO: Does not handle case where base_ospath is literally `/`.
    assert(base_ospath->len > 1);

    // Try to create a canonical module path (where `/` corresponds to main's home directory).
    path_init(dst_path, alloc);

    const char* bp = base_ospath->str;
    const char* sp = src_ospath->str;

    while (*bp && *sp && (*bp == *sp)) {
        bp += 1;
        sp += 1;
    }

    if (*bp) {
        return NIB_PATH_OUTSIDE_ROOT;
    }

    assert(*sp == OS_PATH_SEP);

    path_set(dst_path, sp, (src_ospath->str + src_ospath->len) - sp);
    path_norm(dst_path, OS_PATH_SEP, NIBBLE_PATH_SEP);

    return NIB_PATH_OK;
}
