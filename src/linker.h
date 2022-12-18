#include "nibble.h"
#include "allocator.h"
#include "os_utils.h"
#include "path_utils.h"

ExecCmd get_linker_cmd(Allocator* arena, const BucketList* foreign_libs, const StringView* lib_paths, u32 num_lib_paths,
                       const Path* working_dir, const char* inp_obj, const char* out_path);
