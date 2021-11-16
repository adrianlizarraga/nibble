#ifndef NIBBLE_PATH_UTILS_H
#define NIBBLE_PATH_UTILS_H

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN      // Exclude rarely-used stuff from Windows headers
#include <windows.h>
#include <io.h>
#include <fileapi.h>
#define OS_PATH_SEP '\\'
#define NIBBLE_MAX_PATH MAX_PATH
#else
#include <dirent.h>
#include <limits.h>
#include <sys/stat.h>
#define OS_PATH_SEP '/'
#define NIBBLE_MAX_PATH PATH_MAX
#endif

#define NIBBLE_PATH_SEP '/'

#ifndef NIBBLE_MAX_PATH
#define NIBBLE_MAX_PATH 4096
#warning "Cannot determine maximum path length (PATH_MAX or MAX_PATH)"
#endif

#include "allocator.h"

enum PathFlags
{
    PATH_IS_INVALID = 1 << 0,
    PATH_IS_CANONICAL = 1 << 1,
};

typedef struct Path {
    char* str; // Points to _buf if the path length is < MAX_PATH_LEN
               // Otherwise, points to an allocated buffer.

    size_t len; // Length of str (not counting null character).
    size_t cap; // Total capacity of the buffer pointed to by str.
    Allocator* alloc;
    unsigned flags;
    char _buf[NIBBLE_MAX_PATH];
} Path;

enum DirentFlags
{
    DIRENT_IS_VALID = 1 << 0,
    DIRENT_IS_DIR = 1 << 1,
};

typedef struct DirentIter {
    Path base;
    Path name;
    unsigned flags;
    void* os_handle;
} DirentIter;

typedef enum FileKind
{
    FILE_NONE,
    FILE_REG,
    FILE_DIR,
    FILE_OTHER,
} FileKind;

typedef enum NibblePathErr
{
    NIB_PATH_OK = 0,
    NIB_PATH_INV_PATH,
    NIB_PATH_INV_EXT,
    NIB_PATH_OUTSIDE_ROOT,
} NibblePathErr;

extern const char nib_ext[];

void path_init(Path* path, Allocator* alloc);
void path_norm(Path* path, char old_sep, char new_sep);
void path_free(Path* path);
void path_set(Path* path, const char* src, size_t len);
void path_join(Path* dst, Path* src);
void path_append(Path* dst, const char* str, size_t len);
bool path_abs(Path* path);
char* path_filename(Path* path);
char* path_ext(Path* path);
FileKind path_kind(Path* path);

void dirent_it_init(DirentIter* it, const char* path_str, Allocator* alloc);
void dirent_it_next(DirentIter* it);
void dirent_it_free(DirentIter* it);

void cpath_str_to_ospath(Allocator* alloc, Path* dst, const char* cpath_str, size_t cpath_len, const Path* base_ospath);
NibblePathErr get_import_ospath(Path* import_ospath, const StrLit* import_path_str, const Path* base_ospath,
                                Path* importer_ospath, Allocator* alloc);
NibblePathErr ospath_to_cpath(Path* dst_path, const Path* src_ospath, const Path* base_ospath, Allocator* alloc);
#endif
