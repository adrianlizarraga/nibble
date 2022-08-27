#include "linker.h"
#include "array.h"
#include "compiler.h"

ExecCmd get_linker_cmd(Allocator* arena, const BucketList* foreign_libs, const StringView* lib_paths, u32 num_lib_paths,
                       const char* inp_obj, const char* out_path)
{

    ExecCmd ld_cmd = {.argv = array_create(arena, const char*, 16)};

    u32 num_foreign_libs = foreign_libs->num_elems;

    // Generate a dynamically-linked executable.
    if (num_foreign_libs > 0) {
        const char* dyn_linker_path = "/lib64/ld-linux-x86-64.so.2"; // TODO: Check if exists!

        // EX: ld out.o -o out -pie -dynamic-linker /lib64/ld-linux-x86-64.so.2 -l :libc -L .
        array_push(ld_cmd.argv, "ld");
        array_push(ld_cmd.argv, inp_obj);
        array_push(ld_cmd.argv, "-o");
        array_push(ld_cmd.argv, out_path);
        array_push(ld_cmd.argv, "-pie");

        bool has_shared_lib = false;

        // Push foreign libs in cmd array.
        for (u32 i = 0; i < num_foreign_libs; i++) {
            ForeignLib* lib = (void*)(*bucket_list_get_elem_packed(foreign_libs, i));

            bool is_shared_lib = lib->kind == FOREIGN_LIB_SHARED;
            bool is_static_lib = lib->kind == FOREIGN_LIB_STATIC;

            if (is_shared_lib || is_static_lib) {
                char* libname_arg = array_create(arena, char, lib->name->len + 1);
                ftprint_char_array(&libname_arg, true, ":%s", lib->name->str);

                array_push(ld_cmd.argv, "-l");
                array_push(ld_cmd.argv, libname_arg);
            }
            else {
                assert(lib->kind == FOREIGN_LIB_OBJ);
                array_push(ld_cmd.argv, lib->name->str);
            }

            has_shared_lib = has_shared_lib || is_shared_lib;
        }

        if (has_shared_lib) {
            array_push(ld_cmd.argv, "-dynamic-linker");
            array_push(ld_cmd.argv, dyn_linker_path);
        }

        // Push search dirs into cmd.
        if (num_lib_paths) {
            array_push(ld_cmd.argv, "-L");

            for (u32 i = 0; i < num_lib_paths; i++) {
                const StringView* sp = lib_paths + i;

                array_push(ld_cmd.argv, sp->str);
            }
        }

        array_push(ld_cmd.argv, NULL);

        ld_cmd.argv = ld_cmd.argv;
        ld_cmd.argc = array_len(ld_cmd.argv) - 1;
    }
    // Generate a statically-linked executable.
    else {
        const char* ld_static_cmd[] = {"ld", "-o", out_path, inp_obj, NULL};

        ld_cmd.argc = ARRAY_LEN(ld_static_cmd) - 1;

        for (u32 i = 0; i <= ld_cmd.argc; i++) {
            array_push(ld_cmd.argv, ld_static_cmd[i]);
        }
    }

    return ld_cmd;
}

