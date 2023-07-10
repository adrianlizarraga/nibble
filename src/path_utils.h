#ifndef NIBBLE_PATH_UTILS_H
#define NIBBLE_PATH_UTILS_H

#include <dirent.h>
#include <limits.h>
#include <sys/stat.h>
#define OS_PATH_SEP '/'
#define NIBBLE_MAX_PATH PATH_MAX
#define NIBBLE_PATH_SEP '/'

#ifndef NIBBLE_MAX_PATH
#define NIBBLE_MAX_PATH 4096
#warning "Cannot determine maximum path length (PATH_MAX or MAX_PATH)"
#endif

#include "nibble.h"
#include "array.h"
#include "allocator.h"

typedef enum PathRelativity {
    PATH_REL_INVALID, // Invalid
    PATH_REL_ROOT, // Absolute path: /
    PATH_REL_CURR, // Relative to current directory: ./ or ../
    PATH_REL_PROG_ENTRY, // Relative to program entry dir: $/
    PATH_REL_UNKNOWN, // Relative to unknown path (must resolve with search paths): some_dir/
} PathRelativity;

typedef struct Path {
    char* str; // Stretchy buffer.
} Path;

enum DirentFlags {
    DIRENT_IS_VALID = 1 << 0,
    DIRENT_IS_DIR = 1 << 1,
};

typedef struct DirentIter {
    Path base;
    Path name;
    unsigned flags;
    void* os_handle;
} DirentIter;

typedef enum FileKind {
    FILE_NONE,
    FILE_REG,
    FILE_DIR,
    FILE_OTHER,
} FileKind;

extern const char* nib_ext;
extern const char* shared_lib_ext;
extern const char* static_lib_ext;
extern const char* obj_file_ext;

#define path_len(p) (array_len((p)->str) - 1)
#define path_cap(p) array_cap((p)->str)
#define path_allctr(p) array_allctr((p)->str)

#define PATH_AS_ARGS(p) ((p)->str), path_len(p)

Path path_create(Allocator* allctr, const char* path, u32 len);
Path path_createf(Allocator* allctr, const char* format, ...);
void path_init(Path* dst, Allocator* allctr, const char* path, u32 len);
void path_set(Path* dst, const char* path, u32 len);
void path_free(Path* path);

// The following modify the path argument in place AND return a pointer to the path argument to
// allow chaining.
Path* path_norm(Path* path);
Path* path_join(Path* dst, const char* b, u32 b_len); // dst/b
Path* path_abs(Path* path, const char* cwd_str, u32 cwd_len);

bool path_real(Path* dst, const Path* path);
bool path_isabs(const Path* path);
bool path_str_isabs(const char* path);

const char* path_basename_ptr(const char* path, u32 len);
const char* path_ext_ptr(const char* path, u32 len);
Path path_dirname(Allocator* allctr, const Path* path);

FileKind path_kind(const Path* path);
PathRelativity path_relativity(const char* path, u32 len);

void dirent_it_init(DirentIter* it, const char* path_str, Allocator* alloc);
void dirent_it_next(DirentIter* it);
void dirent_it_free(DirentIter* it);

Path get_curr_dir(Allocator* allctr);
#endif
