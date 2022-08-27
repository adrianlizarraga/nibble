/* TODO:

   - Consider making string literals evaluate to slices (or pointers to static arrays) rather than arrays
   - Only generate assembly for procs reachable from main()
     - Currently, we prune procs not reachable from the main _MODULE_, but unused procs in main module remain.
     - RELATED: Compile all module symbols, but only generate used symbols.
   - Switch statements
   - Floating point primitives
   - Defer statement
*/

/* Wishlist:
    - Anonymous procedures. Can put code in array indexed by an enum kind!
    - Enum sub types. EX: A proc only expects a subset of possible enum values.
    - Programmer should be able to map constants (enums) to other constants (enums) at compile-time, without
      having to create a global/static/readonly mapping array. Need some way of communicating this intent to compiler.
    - Allow constructing/initializing via a procedure using only a single statement.
      Ex: Instead of `var a : MyType; InitMyType(^a);` allow `var a : MyType #init InitMyType();` or `InitMyType(var a : MyType);`
*/

/*
 * BUGS
    - Printing all IRS for floats.nib crashes.
 */

//#define NDEBUG 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//#define NDEBUG
//#define NIBBLE_PRINT_IRS
//#define NIBBLE_PRINT_MEM_USAGE

// This is a "unity build".
// Having a single compilation unit makes building trivial.
// I'll add a proper build system later.
#include "allocator.c"
#include "cstring.c"
#include "path_utils.c"
#include "os_utils.c"
#include "print.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "lexer/module.c"
#include "ast/module.c"
#include "parser/module.c"
#include "resolver/module.c"
#include "bytecode/module.c"
#include "x64_gen/module.c"
#include "nibble.c"

// The maximum number of characters in any path provided by the user.
#define MAX_INPUT_PATH_LEN 1024

// The maximum number of search paths a user can specify via the '-I' option.
#define MAX_NUM_USER_MODULE_PATHS 63

// The number of default search paths added by the compiler.
#define NUM_DEFAULT_MODULE_PATHS 1

// The maximum number of lib/obj search paths a user can specify via the '-L' option.
#define MAX_NUM_USER_LIB_PATHS 63

// The number of default foreign lib/obj search paths added by the compiler.
#define NUM_DEFAULT_LIB_PATHS 1

void print_usage(FILE* fd, const char* program_name)
{
    ftprint_file(fd, true, "Usage: %s [OPTIONS] <input.nib>\n", program_name);
    ftprint_file(fd, true, "OPTIONS:\n");
    ftprint_file(fd, true, "    -h                              Print this help message\n");
    ftprint_file(fd, true, "    -s                              Silent mode (no output to stdout)\n");
    ftprint_file(fd, true, "    -os   [linux | win32 | osx]     Target OS\n");
    ftprint_file(fd, true, "    -arch [x64 | x86]               Target architecture\n");
    ftprint_file(fd, true, "    -I    <module_search_path>      Add module (import/include) search path\n");
    ftprint_file(fd, true, "    -L    <library_search_path>     Add library search path\n");
    ftprint_file(fd, true, "    -o    <output_file>             Output binary file name. Defaults to `out`\n");
}

char* consume_arg(int* argc, char*** argv)
{
    assert(*argc);
    char* arg = (*argv)[0];

    *argc -= 1;
    *argv += 1;

    return arg;
}

char* get_flag_value(int* argc, char*** argv, const char* program_name, const char* flag)
{
    if (*argc == 0) {
        ftprint_err("ERROR: no value provided for `%s` option\n", flag);
        print_usage(stderr, program_name);
        exit(1);
    }

    return consume_arg(argc, argv);
}

OS get_target_os(int* argc, char*** argv, const char* program_name)
{
    OS target_os = OS_INVALID;
    char* os_val = get_flag_value(argc, argv, program_name, "-os");

    cstr_tolower(os_val);

    for (int i = 1; i < NUM_OS; i += 1) {
        if (cstr_cmp(os_names[i], os_val) == 0) {
            target_os = (OS)i;
            break;
        }
    }

    if (target_os == OS_INVALID) {
        ftprint_err("ERROR: Invalid OS `%s`\n", os_val);
        print_usage(stderr, program_name);
        exit(1);
    }

    return target_os;
}

Arch get_target_arch(int* argc, char*** argv, const char* program_name)
{
    Arch target_arch = ARCH_INVALID;
    char* arch_val = get_flag_value(argc, argv, program_name, "-arch");

    cstr_tolower(arch_val);

    for (int i = 1; i < NUM_ARCH; i += 1) {
        if (cstr_cmp(arch_names[i], arch_val) == 0) {
            target_arch = (Arch)i;
            break;
        }
    }

    if (target_arch == ARCH_INVALID) {
        ftprint_err("ERROR: Invalid architecture `%s`\n", arch_val);
        print_usage(stderr, program_name);
        exit(1);
    }

    return target_arch;
}

void check_input_filepath(StringView path, const char* program_name)
{
    if (path.len > MAX_INPUT_PATH_LEN) {
        ftprint_err("ERROR: input (%.*s...) is too long (maximum of %u characters)\n",
                    MAX_INPUT_PATH_LEN >> 5, path.str, MAX_INPUT_PATH_LEN);
        print_usage(stderr, program_name);
        exit(1);
    }
}

void add_search_path(StringView* paths, u32* p_num_paths, u32 cap, const char* new_path, const char* prog_name, const char* type)
{
    if (*p_num_paths >= cap) {
        ftprint_err("ERROR: Too many %s search paths (maximum of %u)\n", type, cap);
        print_usage(stderr, prog_name);
        exit(1);
    }

    StringView sp = string_view(new_path);
    check_input_filepath(sp, prog_name);

    paths[*p_num_paths] = sp;
    *p_num_paths += 1;
}

int main(int argc, char* argv[])
{
    StringView module_paths[MAX_NUM_USER_MODULE_PATHS + NUM_DEFAULT_MODULE_PATHS];
    StringView lib_paths[MAX_NUM_USER_LIB_PATHS + NUM_DEFAULT_LIB_PATHS];

    u32 num_module_paths = 0;
    u32 num_lib_paths = 0;

    const char* program_name = consume_arg(&argc, &argv);
    StringView main_file = {0};
    StringView out_file = string_view_lit("out");

    bool silent = false;

    // Set default target os/arch.
#if defined(NIBBLE_HOST_LINUX)
    OS target_os = OS_LINUX;
    Arch target_arch = ARCH_X64;
#else
    OS target_os = OS_INVALID;
    Arch target_arch = ARCH_INVALID;
#endif

    while (argc > 0) {
        const char* arg = consume_arg(&argc, &argv);

        if (cstr_cmp(arg, "-h") == 0) {
            print_usage(stdout, program_name);
            exit(0);
        }
        else if (cstr_cmp(arg, "-s") == 0) {
            silent = true;
        }
        else if (cstr_cmp(arg, "-os") == 0) {
            target_os = get_target_os(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-arch") == 0) {
            target_arch = get_target_arch(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-o") == 0) {
            const char* name = get_flag_value(&argc, &argv, program_name, "-o");
            out_file = string_view(name);

            check_input_filepath(out_file, program_name);
        }
        else if (cstr_cmp(arg, "-I") == 0) {
            const char* search_path = get_flag_value(&argc, &argv, program_name, "-I");

            add_search_path(module_paths, &num_module_paths, MAX_NUM_USER_MODULE_PATHS, search_path, program_name, "module");
        }
        else if (cstr_cmp(arg, "-L") == 0) {
            const char* search_path = get_flag_value(&argc, &argv, program_name, "-L");

            add_search_path(lib_paths, &num_lib_paths, MAX_NUM_USER_LIB_PATHS, search_path, program_name, "library");
        }
        else {
            if (main_file.len) {
                ftprint_err("[ERROR]: unknown option `%s`\n\n", arg);
                print_usage(stderr, program_name);
                exit(1);
            }

            main_file = string_view(arg);
            check_input_filepath(main_file, program_name);
        }
    }

    if (!main_file.len) {
        ftprint_err("[ERROR]: No input source file provided.\n\n");
        print_usage(stderr, program_name);
        exit(1);
    }

    if (!out_file.len) {
        ftprint_err("[ERROR]: Invalid output file name provided.\n\n");
        print_usage(stderr, program_name);
        exit(1);
    }

    Allocator arena = allocator_create(65536);
    Path working_dir = get_curr_dir(&arena);

    // Validate output file's path and get output directory.
    Path out_path = path_create(&arena, out_file.str, out_file.len);
    path_abs(&out_path, PATH_AS_ARGS(&working_dir));
    Path out_dir_path = path_dirname(&arena, &out_path);

    if (path_kind(&out_dir_path) != FILE_DIR) {
        ftprint_err("[ERROR]: Output path `%s` is invalid\n", out_dir_path.str);
        allocator_destroy(&arena);
        exit(1);
    }

    // Validate main file path and get program entry directory.
    Path main_path = path_create(&arena, main_file.str, main_file.len);
    path_abs(&main_path, PATH_AS_ARGS(&working_dir));
    FileKind main_file_kind = path_kind(&main_path);

    if (main_file_kind == FILE_NONE) {
        ftprint_err("[ERROR]: Cannot find file `%s`\n", main_file.str);
        allocator_destroy(&arena);
        exit(1);
    }

    if ((main_file_kind != FILE_REG) || cstr_cmp(path_ext_ptr(PATH_AS_ARGS(&main_path)), nib_ext) != 0) {
        ftprint_err("[ERROR]: Program entry file `%s` is not a valid `.nib` source file.\n", main_path.str);
        allocator_destroy(&arena);
        exit(1);
    }

    Path prog_entry_dir = path_dirname(&arena, &main_path);

    // Add default search paths.
    add_search_path(module_paths, &num_module_paths, ARRAY_LEN(module_paths), ".", program_name, "module");
    add_search_path(lib_paths, &num_lib_paths, ARRAY_LEN(lib_paths), out_dir_path.str, program_name, "library");

    NibbleCtx* nib_ctx = nibble_init(&arena, target_os, target_arch, silent,
                                     &working_dir, &prog_entry_dir,
                                     module_paths, num_module_paths, lib_paths, num_lib_paths);

    if (!nib_ctx) {
        ftprint_err("[ERROR]: Failed to initialize compiler.\n");
        allocator_destroy(&arena);
        exit(1);
    }

    bool success = nibble_compile(nib_ctx, &main_path, &out_path);
    nibble_cleanup(nib_ctx);
    allocator_destroy(&arena);

#ifdef NIBBLE_PRINT_MEM_USAGE
    ftprint_out("heap usage: %u allocs, %u frees, %lu bytes allocated\n", nib_alloc_count, nib_free_count, nib_alloc_size);

    if (nib_free_count != nib_alloc_count) {
        ftprint_out("[ERROR]: MEMORY LEAK DETECTED\n");
    }
#endif

    return !success;
}
