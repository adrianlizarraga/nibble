#include "linker.h"
#include "array.h"
#include "compiler.h"

ExecCmd get_linker_cmd(Allocator* arena, const BucketList* foreign_libs, const StringView* lib_paths, u32 num_lib_paths,
                       const Path* working_dir, const char* inp_obj, const char* out_path)
{
    // EX: ld out.o -o out -pie -dynamic-linker /lib64/ld-linux-x86-64.so.2 -l :libc -L .
    ExecCmd ld_cmd = {.argv = array_create(arena, const char*, 16)};

    array_push(ld_cmd.argv, "ld");
    array_push(ld_cmd.argv, inp_obj);

    bool has_shared_lib = false;
    bool has_static_lib = false;

    u32 num_foreign_libs = foreign_libs->num_elems;

    // Process foreign code: shared libs, static libs, and object files.
    for (u32 i = 0; i < num_foreign_libs; i++) {
        ForeignLib* lib = (void*)(*bucket_list_get_elem_packed(foreign_libs, i));

        bool is_shared_lib = lib->kind == FOREIGN_LIB_SHARED;
        bool is_static_lib = lib->kind == FOREIGN_LIB_STATIC;

        if (is_shared_lib || is_static_lib) {
            // Add library as ld argument. Ex: ld -l :libmycode.a -l :libc.so ....
            char* libname_arg = alloc_array(arena, char, lib->name->len + 2, false);

            libname_arg[0] = ':';
            memcpy(&libname_arg[1], lib->name->str, lib->name->len);
            libname_arg[lib->name->len + 1] = '\0';

            array_push(ld_cmd.argv, "-l");
            array_push(ld_cmd.argv, libname_arg);
        }
        else {
            // Find the filepath of the object file and add it as an `ld` argument.
            // Ex: ld /<some_path/obj.o .....
            assert(lib->kind == FOREIGN_LIB_OBJ);
            Path obj_path = path_create(arena, NULL, 0);
            FileKind obj_file_kind = FILE_NONE;

            for (u32 j = 0; j < num_lib_paths; j += 1) {
                path_set(&obj_path, lib_paths[j].str, lib_paths[j].len); // Init to search path.
                path_abs(path_join(&obj_path, lib->name->str, lib->name->len), PATH_AS_ARGS(working_dir));

                obj_file_kind = path_kind(&obj_path);

                if (obj_file_kind == FILE_REG) {
                    break;
                }
            }

            if (obj_file_kind != FILE_REG) {
                ftprint_err("[ERROR]: Cannot find foreign object file %s\n", lib->name->str);
                return (ExecCmd){0};
            }

            array_push(ld_cmd.argv, obj_path.str);
        }

        has_shared_lib = has_shared_lib || is_shared_lib;
        has_static_lib = has_static_lib || is_static_lib;
    }

    if (has_shared_lib) {
        array_push(ld_cmd.argv, "-pie");
        array_push(ld_cmd.argv, "-dynamic-linker");
        array_push(ld_cmd.argv, "/lib64/ld-linux-x86-64.so.2");
    }

    // Push search dirs into cmd.
    if (num_lib_paths && (has_shared_lib || has_static_lib)) {
        for (u32 i = 0; i < num_lib_paths; i++) {
            array_push(ld_cmd.argv, "-L");
            array_push(ld_cmd.argv, lib_paths[i].str);
        }
    }

    array_push(ld_cmd.argv, "-o");
    array_push(ld_cmd.argv, out_path);
    array_push(ld_cmd.argv, NULL);

    ld_cmd.argv = ld_cmd.argv;
    ld_cmd.argc = array_len(ld_cmd.argv) - 1;

    return ld_cmd;
}

