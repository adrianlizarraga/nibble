#include <assert.h>
#include <stdlib.h>

#include "compiler.h"
#include "cstring.h"
#include "parser/module.h"
#include "resolver/module.h"
#include "bytecode/module.h"
#include "x64_gen/module.h"
#include "path_utils.h"
#include "linker.h"
#include "os_utils.h"

typedef struct SourceFile {
    ProgRange range;
    const char* code;
    const char* abs_path;
    size_t abs_path_len;

    ProgPos* line_pos; // Stretchy array for now
} SourceFile;

static inline ProgPos get_curr_src_pos(BucketList* src_files)
{
    if (!src_files->num_elems) {
        return 0;
    }

    SourceFile* prev_file = (SourceFile*)(*bucket_list_get_last_packed(src_files));
    assert(prev_file);

    return prev_file->range.end;
}

static SourceFile* add_src_file(BucketList* src_files, const char* abs_path, size_t abs_path_len, const char* code, size_t code_len)
{
    ProgPos src_pos = get_curr_src_pos(src_files);
    SourceFile* src_file = alloc_type(src_files->arena, SourceFile, true);
    src_file->range.start = src_pos;
    src_file->range.end = src_pos + (ProgPos)code_len;
    src_file->code = code;
    src_file->abs_path = abs_path;
    src_file->abs_path_len = abs_path_len;
    src_file->line_pos = array_create(src_files->arena, ProgPos, 32);

    bucket_list_add_elem(src_files, src_file);

    return src_file;
}

static SourceFile* get_src_file(BucketList* src_files, ProgPos pos)
{
    for (Bucket* bucket = src_files->first; bucket; bucket = bucket->next) {
        for (size_t i = 0; i < bucket->count; i += 1) {
            SourceFile* src_file = bucket->elems[i];

            if (pos < src_file->range.end) {
                return src_file;
            }
        }
    }

    return NULL;
}

typedef struct LineCol {
    unsigned line;
    unsigned col;
} LineCol;

static LineCol get_src_linecol(SourceFile* src_file, ProgPos pos)
{
    assert(pos >= src_file->range.start);
    assert(pos <= src_file->range.end);

    LineCol result = {0};

    size_t num_line_pos = array_len(src_file->line_pos);
    size_t line_pos_i = 0;

    for (; line_pos_i < num_line_pos; line_pos_i += 1) {
        ProgPos line_end = src_file->line_pos[line_pos_i];
        assert(line_end <= src_file->range.end);

        if (pos < line_end) {
            break;
        }
    }

    ProgPos line_start = line_pos_i == 0 ? src_file->range.start : src_file->line_pos[line_pos_i - 1];

    result.line = line_pos_i + 1;
    result.col = pos - line_start + 1;

    return result;
}

#define RED_COLOR_CODE "\x1B[31m"
#define RESET_COLOR_CODE "\x1B[0m"

static void print_error(BucketList* src_files, Error* error, bool use_colors, bool test_mode_paths)
{
    SourceFile* src_file = get_src_file(src_files, error->range.start);
    assert(src_file);

    LineCol linecol_s = get_src_linecol(src_file, error->range.start);

    const char* src_file_path = src_file->abs_path;
    size_t src_file_path_len = src_file->abs_path_len;

    if (test_mode_paths) {
        src_file_path = path_basename_ptr(src_file_path, (u32)src_file_path_len);
        src_file_path_len = src_file->abs_path_len - (src_file_path - src_file->abs_path);
    }

    ftprint_err("%.*s:%u:%u: [Error]: %s\n\n", src_file_path_len, src_file_path, linecol_s.line, linecol_s.col, error->msg);

    unsigned line = linecol_s.line;

    ftprint_err(" %5d | ", line);

    for (ProgPos p = (error->range.start - (linecol_s.col - 1)); p < error->range.start; p += 1) {
        size_t i = p - src_file->range.start;
        char ch = src_file->code[i];

        if (ch == '\n') {
            line += 1;
            ftprint_err("\n %5d | ", line);
        }
        else {
            ftprint_err("%c", src_file->code[i]);
        }
    }

    if (use_colors) {
        ftprint_err(RED_COLOR_CODE);
    }

    for (ProgPos p = error->range.start; p < error->range.end; p += 1) {
        size_t i = p - src_file->range.start;
        char ch = src_file->code[i];

        if (ch == '\n') {
            line += 1;

            if (use_colors) {
                ftprint_err(RESET_COLOR_CODE "\n %5d | " RED_COLOR_CODE, line);
            }
            else {
                ftprint_err("\n %5d | ", line);
            }
        }
        else {
            ftprint_err("%c", src_file->code[i]);
        }
    }

    if (use_colors) {
        ftprint_err(RESET_COLOR_CODE);
    }

    const char* p = src_file->code + (error->range.end - src_file->range.start);

    while (*p && (*p != '\n')) {
        ftprint_err("%c", *p);
        p += 1;
    }

    ftprint_err("\n\n");
}

static void print_errors(NibbleCtx* nib_ctx)
{
    ErrorStream* errors = &nib_ctx->errors;

    if (errors->count > 0) {
        const bool use_colors = is_stderr_atty();
        const bool test_mode_paths = nib_ctx->test_mode_paths;

        ftprint_err("\n%u errors:\n\n", errors->count);
        Error* err = errors->first;

        while (err) {
            print_error(&nib_ctx->src_files, err, use_colors, test_mode_paths);
            err = err->next;
        }
    }
}

static void print_info(NibbleCtx* nib_ctx, const char* format, ...)
{
    if (nib_ctx->silent) {
        return;
    }

    va_list vargs;

    ftprint_out("[INFO]: ");

    va_start(vargs, format);
    ftprintv_out(format, vargs);
    va_end(vargs);

    ftprint_out("\n");
}

typedef enum NibblePathErr {
    NIB_PATH_OK = 0,
    NIB_PATH_INV_PATH,
    NIB_PATH_INV_EXT,
} NibblePathErr;

static NibblePathErr get_import_abspath(Path* result, const StrLit* import_path_str, const Path* importer_ospath,
                                        const Path* working_dir, const Path* prog_entry_dir, const StringView* module_paths,
                                        u32 num_module_paths, Allocator* alloc)
{
    assert(path_isabs(importer_ospath));

    StringView dir_module_entry = string_view_lit("module.nib");
    PathRelativity rel = path_relativity(import_path_str->str, import_path_str->len);

    switch (rel) {
    case PATH_REL_ROOT: // Import path is absolute.
        path_init(result, alloc, import_path_str->str, import_path_str->len);
        path_norm(result);

        assert(path_isabs(result));

        FileKind file_kind = path_kind(result);

        // If import points to a directory, modify path to <dir>/module.nib
        if (file_kind == FILE_DIR) {
            path_join(result, dir_module_entry.str, dir_module_entry.len);
            file_kind = path_kind(result);
        }

        // Check if file's path exists somewhere.
        if (file_kind != FILE_REG) {
            return NIB_PATH_INV_PATH;
        }

        // Check for .nib extension.
        if (cstr_cmp(path_ext_ptr(PATH_AS_ARGS(result)), nib_ext) != 0) {
            return NIB_PATH_INV_EXT;
        }

        return NIB_PATH_OK;
    case PATH_REL_CURR: { // Import path is relative to importer.
        const char* dir_begp = importer_ospath->str;
        const char* dir_endp = path_basename_ptr(PATH_AS_ARGS(importer_ospath)) - 1;

        path_init(result, alloc, dir_begp, (dir_endp - dir_begp));
        path_norm(path_join(result, import_path_str->str, import_path_str->len));

        assert(path_isabs(result));

        FileKind file_kind = path_kind(result);

        // If import points to a directory, modify path to <dir>/module.nib
        if (file_kind == FILE_DIR) {
            path_join(result, dir_module_entry.str, dir_module_entry.len);
            file_kind = path_kind(result);
        }

        // Check if file's path exists somewhere.
        if (file_kind != FILE_REG) {
            return NIB_PATH_INV_PATH;
        }

        // Check for .nib extension.
        if (cstr_cmp(path_ext_ptr(PATH_AS_ARGS(result)), nib_ext) != 0) {
            return NIB_PATH_INV_EXT;
        }

        return NIB_PATH_OK;
    }
    case PATH_REL_PROG_ENTRY: {
        assert(import_path_str->len >= 2);

        // Strip away the beginning '$/' because it is not standard.
        const char* rel_start = import_path_str->str + 2;
        u32 rel_len = import_path_str->len - 2;

        path_init(result, alloc, PATH_AS_ARGS(prog_entry_dir));
        path_norm(path_join(result, rel_start, rel_len));

        assert(path_isabs(result));

        FileKind file_kind = path_kind(result);

        // If import points to a directory, modify path to <dir>/module.nib
        if (file_kind == FILE_DIR) {
            path_join(result, dir_module_entry.str, dir_module_entry.len);
            file_kind = path_kind(result);
        }

        // Check if file's path exists somewhere.
        if (file_kind != FILE_REG) {
            return NIB_PATH_INV_PATH;
        }

        // Check for .nib extension.
        if (cstr_cmp(path_ext_ptr(PATH_AS_ARGS(result)), nib_ext) != 0) {
            return NIB_PATH_INV_EXT;
        }

        return NIB_PATH_OK;
    }
    case PATH_REL_UNKNOWN: {
        FileKind file_kind = FILE_NONE;
        const char* file_ext = NULL;

        path_init(result, alloc, NULL, 0);

        for (u32 i = 0; i < num_module_paths; i += 1) {
            const StringView* sp = module_paths + i;

            path_set(result, sp->str, sp->len); // Initialize to the search path.
            path_abs(path_join(result, import_path_str->str, import_path_str->len), PATH_AS_ARGS(working_dir));

            file_kind = path_kind(result);

            // If import points to a directory, modify path to <dir>/module.nib
            if (file_kind == FILE_DIR) {
                path_join(result, dir_module_entry.str, dir_module_entry.len);
                file_kind = path_kind(result);
            }

            file_ext = path_ext_ptr(PATH_AS_ARGS(result));

            // Return if the .nib file exists.
            if ((file_kind == FILE_REG) && (cstr_cmp(file_ext, nib_ext) == 0)) {
                assert(path_isabs(result));
                return NIB_PATH_OK;
            }
        }

        return file_kind == FILE_REG ? NIB_PATH_INV_EXT : NIB_PATH_INV_PATH;
    }
    default:
        return NIB_PATH_INV_PATH;
    }
}

static bool init_annotations(HMap* ident_map)
{
    static const StringView names[ANNOTATION_COUNT] = {
        [ANNOTATION_EXPORTED] = string_view_lit("exported"),
        [ANNOTATION_FOREIGN] = string_view_lit("foreign"),
        [ANNOTATION_PACKED] = string_view_lit("packed"),
    };

    for (int i = 0; i < ANNOTATION_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(ident_map, str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        annotation_names[i] = ident->str;
    }

    return true;
}

static bool init_builtin_struct_fields(HMap* ident_map)
{
    static const StringView names[BUILTIN_STRUCT_FIELD_COUNT] = {
        [BUILTIN_STRUCT_FIELD_LENGTH] = string_view_lit("length"),
        [BUILTIN_STRUCT_FIELD_DATA] = string_view_lit("data"),
        [BUILTIN_STRUCT_FIELD_TYPE] = string_view_lit("type"),
        [BUILTIN_STRUCT_FIELD_PTR] = string_view_lit("ptr"),
    };

    for (int i = 0; i < BUILTIN_STRUCT_FIELD_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(ident_map, str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        builtin_struct_fields[i] = ident;
    }

    return true;
}

/*
// Code for the builtin module that is automatically imported everywhere:
// Generated by: xxd -i builtin.nib
// NOTE: A terminating null character has to be added to the output of xxd

proc #writeout(buf: ^char, size: usize) => ssize;
proc #readin(buf: ^char, size: usize) => ssize;
proc #memcpy(dst: ^void, src: ^void, size: usize);
proc #memset(dst: ^void, value: uchar, size: usize);
proc #syscall0(syscall_nr : ssize) => ssize;
proc #syscall1(syscall_nr : ssize, arg1 : ssize) => ssize;
proc #syscall2(syscall_nr : ssize, arg1 : ssize, arg2 : ssize) => ssize;
proc #syscall3(syscall_nr : ssize, arg1 : ssize, arg2 : ssize, arg3 : ssize) => ssize;
proc #syscall4(syscall_nr : ssize, arg1 : ssize, arg2 : ssize, arg3 : ssize, arg4 : ssize) => ssize;
proc #syscall5(syscall_nr : ssize, arg1 : ssize, arg2 : ssize, arg3 : ssize, arg4 : ssize, arg5 : ssize) => ssize;
proc #syscall6(syscall_nr : ssize, arg1 : ssize, arg2 : ssize, arg3 : ssize, arg4 : ssize, arg5 : ssize, arg6 : ssize) => ssize;
*/
static const char builtin_code[] = {
    0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x77, 0x72, 0x69, 0x74, 0x65, 0x6f, 0x75, 0x74, 0x28, 0x62, 0x75, 0x66, 0x3a, 0x20, 0x5e, 0x63,
    0x68, 0x61, 0x72, 0x2c, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73,
    0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x72, 0x65, 0x61, 0x64, 0x69, 0x6e, 0x28, 0x62, 0x75, 0x66,
    0x3a, 0x20, 0x5e, 0x63, 0x68, 0x61, 0x72, 0x2c, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20,
    0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x6d, 0x65, 0x6d, 0x63, 0x70, 0x79,
    0x28, 0x64, 0x73, 0x74, 0x3a, 0x20, 0x5e, 0x76, 0x6f, 0x69, 0x64, 0x2c, 0x20, 0x73, 0x72, 0x63, 0x3a, 0x20, 0x5e, 0x76, 0x6f, 0x69,
    0x64, 0x2c, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20,
    0x23, 0x6d, 0x65, 0x6d, 0x73, 0x65, 0x74, 0x28, 0x64, 0x73, 0x74, 0x3a, 0x20, 0x5e, 0x76, 0x6f, 0x69, 0x64, 0x2c, 0x20, 0x76, 0x61,
    0x6c, 0x75, 0x65, 0x3a, 0x20, 0x75, 0x63, 0x68, 0x61, 0x72, 0x2c, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a,
    0x65, 0x29, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x30, 0x28, 0x73, 0x79, 0x73,
    0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x6e, 0x72, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73,
    0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x31, 0x28, 0x73, 0x79,
    0x73, 0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x6e, 0x72, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x31,
    0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72,
    0x6f, 0x63, 0x20, 0x23, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x32, 0x28, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x6e,
    0x72, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x31, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a,
    0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x32, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73,
    0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x33, 0x28, 0x73, 0x79,
    0x73, 0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x6e, 0x72, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x31,
    0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x32, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65,
    0x2c, 0x20, 0x61, 0x72, 0x67, 0x33, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69,
    0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x34, 0x28, 0x73, 0x79, 0x73,
    0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x6e, 0x72, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x31, 0x20,
    0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x32, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c,
    0x20, 0x61, 0x72, 0x67, 0x33, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x34, 0x20, 0x3a, 0x20,
    0x73, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20,
    0x23, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x35, 0x28, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x6e, 0x72, 0x20, 0x3a,
    0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x31, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20,
    0x61, 0x72, 0x67, 0x32, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x33, 0x20, 0x3a, 0x20, 0x73,
    0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x34, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72,
    0x67, 0x35, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a,
    0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c, 0x36, 0x28, 0x73, 0x79, 0x73, 0x63, 0x61, 0x6c, 0x6c,
    0x5f, 0x6e, 0x72, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x31, 0x20, 0x3a, 0x20, 0x73, 0x73,
    0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x32, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67,
    0x33, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x34, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a,
    0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x35, 0x20, 0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x2c, 0x20, 0x61, 0x72, 0x67, 0x36, 0x20,
    0x3a, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x00};

static bool init_intrinsics(HMap* ident_map)
{
    static const StringView names[INTRINSIC_COUNT] = {
        [INTRINSIC_READIN] = string_view_lit("#readin"),     [INTRINSIC_WRITEOUT] = string_view_lit("#writeout"),
        [INTRINSIC_MEMCPY] = string_view_lit("#memcpy"),     [INTRINSIC_MEMSET] = string_view_lit("#memset"),
        [INTRINSIC_SYSCALL0] = string_view_lit("#syscall0"), [INTRINSIC_SYSCALL1] = string_view_lit("#syscall1"),
        [INTRINSIC_SYSCALL2] = string_view_lit("#syscall2"), [INTRINSIC_SYSCALL3] = string_view_lit("#syscall3"),
        [INTRINSIC_SYSCALL4] = string_view_lit("#syscall4"), [INTRINSIC_SYSCALL5] = string_view_lit("#syscall5"),
        [INTRINSIC_SYSCALL6] = string_view_lit("#syscall6"),
    };

    for (int i = 0; i < INTRINSIC_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(ident_map, str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        ident->kind = IDENTIFIER_INTRINSIC;
        ident->intrinsic = (Intrinsic)i;

        intrinsic_idents[i] = ident;
    }

    return true;
}

static bool init_keywords(HMap* ident_map)
{
    static const StringView names[KW_COUNT] = {[KW_VAR] = string_view_lit("var"),
                                               [KW_CONST] = string_view_lit("const"),
                                               [KW_ENUM] = string_view_lit("enum"),
                                               [KW_UNION] = string_view_lit("union"),
                                               [KW_STRUCT] = string_view_lit("struct"),
                                               [KW_PROC] = string_view_lit("proc"),
                                               [KW_TYPEDEF] = string_view_lit("typedef"),
                                               [KW_SIZEOF] = string_view_lit("#sizeof"),
                                               [KW_TYPEOF] = string_view_lit("#typeof"),
                                               [KW_TYPEID] = string_view_lit("#typeid"),
                                               [KW_INDEXOF] = string_view_lit("#indexof"),
                                               [KW_OFFSETOF] = string_view_lit("#offsetof"),
                                               [KW_LENGTH] = string_view_lit("#length"),
                                               [KW_STATIC_ASSERT] = string_view_lit("#static_assert"),
                                               [KW_RET_TYPE] = string_view_lit("#ret_type"),
                                               [KW_EXPORT] = string_view_lit("export"),
                                               [KW_IMPORT] = string_view_lit("import"),
                                               [KW_FROM] = string_view_lit("from"),
                                               [KW_AS] = string_view_lit("as"),
                                               [KW_INCLUDE] = string_view_lit("#include"),
                                               [KW_LABEL] = string_view_lit("label"),
                                               [KW_GOTO] = string_view_lit("goto"),
                                               [KW_BREAK] = string_view_lit("break"),
                                               [KW_CONTINUE] = string_view_lit("continue"),
                                               [KW_RETURN] = string_view_lit("return"),
                                               [KW_IF] = string_view_lit("if"),
                                               [KW_ELSE] = string_view_lit("else"),
                                               [KW_WHILE] = string_view_lit("while"),
                                               [KW_DO] = string_view_lit("do"),
                                               [KW_FOR] = string_view_lit("for"),
                                               [KW_SWITCH] = string_view_lit("switch"),
                                               [KW_CASE] = string_view_lit("case"),
                                               [KW_UNDERSCORE] = string_view_lit("_"),
                                               [KW_TRUE] = string_view_lit("true"),
                                               [KW_FALSE] = string_view_lit("false"),
                                               [KW_NULL] = string_view_lit("null")};

    for (int i = 0; i < KW_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(ident_map, str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        ident->kind = IDENTIFIER_KEYWORD;
        ident->kw = (Keyword)i;

        keyword_names[i] = ident->str;
    }

    return true;
}

NibbleCtx* nibble_init(Allocator* mem_arena, OS target_os, Arch target_arch, bool silent, bool test_mode_paths, bool gen_asm,
                       const Path* working_dir, const Path* prog_entry_dir, const StringView* module_paths, u32 num_module_paths,
                       const StringView* lib_paths, u32 num_lib_paths)
{
    static NibbleCtx* nib_ctx;
    static const char main_name[] = "main";

    // First, check host/target archs
#ifdef NIBBLE_HOST_LINUX
    if (target_arch != ARCH_X64 || target_os != OS_LINUX) {
        ftprint_err("[ERROR]: Target OS (%s %s) not yet supported on Linux.\n", os_names[target_os], arch_names[target_arch]);
        return false;
    }
#else
    ftprint_err("[ERROR]: Target OS (%s %s) not supported on `Unknown` OS.\n", os_names[target_os], arch_names[target_arch]);
    exit(1);
#endif

    nib_ctx = alloc_type(mem_arena, NibbleCtx, true);

    nib_ctx->silent = silent;
    nib_ctx->test_mode_paths = test_mode_paths;
    nib_ctx->gen_asm = gen_asm;
    nib_ctx->target_os = target_os;
    nib_ctx->target_arch = target_arch;
    nib_ctx->gen_mem = mem_arena;
    nib_ctx->ast_mem = allocator_create(65536);
    nib_ctx->tmp_mem = allocator_create(32768);
    nib_ctx->str_lit_map = hmap(8, nib_ctx->gen_mem);
    nib_ctx->float_lit_map = hmap(8, nib_ctx->gen_mem);
    nib_ctx->ident_map = hmap(8, nib_ctx->gen_mem);
    nib_ctx->mod_map = hmap(8, &nib_ctx->ast_mem);
    nib_ctx->type_cache.ptrs = hmap(8, &nib_ctx->ast_mem);
    nib_ctx->type_cache.arrays = hmap(8, &nib_ctx->ast_mem);
    nib_ctx->type_cache.procs = hmap(8, &nib_ctx->ast_mem);
    nib_ctx->type_cache.slices = hmap(8, &nib_ctx->ast_mem);
    nib_ctx->type_cache.structs = hmap(8, &nib_ctx->ast_mem);
    nib_ctx->type_cache.unions = hmap(8, &nib_ctx->ast_mem);
    nib_ctx->working_dir = working_dir;
    nib_ctx->prog_entry_dir = prog_entry_dir;
    nib_ctx->module_paths = module_paths;
    nib_ctx->num_module_paths = num_module_paths;
    nib_ctx->lib_paths = lib_paths;
    nib_ctx->num_lib_paths = num_lib_paths;

    if (!path_isabs(nib_ctx->working_dir)) {
        return NULL;
    }

    if (!init_keywords(&nib_ctx->ident_map)) {
        return NULL;
    }

    if (!init_intrinsics(&nib_ctx->ident_map)) {
        return NULL;
    }

    if (!init_annotations(&nib_ctx->ident_map)) {
        return NULL;
    }

    if (!init_builtin_struct_fields(&nib_ctx->ident_map)) {
        return NULL;
    }

    main_proc_ident = intern_ident(&nib_ctx->ident_map, main_name, sizeof(main_name) - 1);

    assert(nib_ctx->ident_map.len == (KW_COUNT + ANNOTATION_COUNT + INTRINSIC_COUNT + BUILTIN_STRUCT_FIELD_COUNT + 1));

    bucket_list_init(&nib_ctx->src_files, nib_ctx->gen_mem, 16);
    bucket_list_init(&nib_ctx->vars.list, &nib_ctx->ast_mem, 32);
    bucket_list_init(&nib_ctx->procs, &nib_ctx->ast_mem, 32);
    bucket_list_init(&nib_ctx->aggregate_types, &nib_ctx->ast_mem, 16);
    bucket_list_init(&nib_ctx->str_lits.list, &nib_ctx->ast_mem, 8);
    bucket_list_init(&nib_ctx->float_lits.list, &nib_ctx->ast_mem, 8);

    nib_ctx->foreign_lib_map = hmap(3, nib_ctx->gen_mem);
    bucket_list_init(&nib_ctx->foreign_libs, nib_ctx->gen_mem, 8);
    bucket_list_init(&nib_ctx->foreign_procs, &nib_ctx->ast_mem, 8);

    error_stream_init(&nib_ctx->errors, nib_ctx->gen_mem);

    init_builtin_types(target_os, target_arch, &nib_ctx->ast_mem, &nib_ctx->type_cache);

    return nib_ctx;
}

static Module* add_module(HMap* mod_map, StrLit* abs_path)
{
    Module* mod = alloc_type(mod_map->allocator, Module, true);

    module_init(mod, mod_map->len, abs_path);
    hmap_put(mod_map, PTR_UINT(abs_path), PTR_UINT(mod));

    return mod;
}

static Module* get_module(HMap* mod_map, StrLit* abs_path)
{
    uint64_t* pval = hmap_get(mod_map, PTR_UINT(abs_path));
    Module* mod = pval ? (void*)*pval : NULL;

    return mod;
}

static void init_builtin_syms(Allocator* ast_mem, Module* builtin_module, HMap* ident_map)
{
    size_t num_types = ARRAY_LEN(builtin_types);

    for (size_t i = 0; i < num_types; i += 1) {
        BuiltinType* builtin = builtin_types + i;

        Identifier* interned_name = intern_ident(ident_map, builtin->name, cstr_len(builtin->name));

        if (lookup_scope_symbol(&builtin_module->scope, interned_name)) {
            NIBBLE_FATAL_EXIT("[INTERNAL ERROR] Duplicate definition of builtin symbol `%s`", interned_name->str);
            return;
        }

        Symbol* sym = new_symbol_builtin_type(ast_mem, interned_name, builtin->type, builtin_module);

        add_scope_symbol(&builtin_module->scope, interned_name, sym, true);
    }
}

#define NIBBLE_INCLUDE_LIMIT 50

typedef struct CachedInclude {
    struct CachedInclude* next;
    Path* includer_ospath;
    size_t len;
    char str[];
} CachedInclude;

static bool parse_code_recursive(NibbleCtx* ctx, Module* mod, const char* abs_path, size_t abs_path_len, const char* code,
                                 size_t code_len, HMap* seen_includes, int include_depth)
{
    ProgPos src_pos = get_curr_src_pos(&ctx->src_files);
    SourceFile* src_file = add_src_file(&ctx->src_files, abs_path, abs_path_len, code, code_len);

    Lexer lexer = {.str = code,
                   .at = code,
                   .start = src_pos,
                   .arena = &ctx->tmp_mem,
                   .errors = &ctx->errors,
                   .line_pos = &src_file->line_pos,
                   .ident_map = &ctx->ident_map,
                   .str_lit_map = &ctx->str_lit_map};
    Parser parser = {.ast_arena = &ctx->ast_mem, .errors = &ctx->errors, .lexer = &lexer};
    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF)) {
        Stmt* stmt = parse_global_stmt(&parser);

        if (!stmt || ctx->errors.count)
            return false;

        if (stmt->kind == CST_StmtInclude) {
            StmtInclude* stmt_include = (StmtInclude*)stmt;

            Path file_ospath = path_create(&ctx->tmp_mem, abs_path, abs_path_len);

            if (include_depth > NIBBLE_INCLUDE_LIMIT) {
                report_error(&ctx->errors, stmt->range,
                             "Include limit exceeded. File include chain exceeded the current threshold of `%d`.",
                             NIBBLE_INCLUDE_LIMIT);
                return false;
            }

            Path include_ospath;
            NibblePathErr ret = get_import_abspath(&include_ospath, stmt_include->file_pathname, &file_ospath, ctx->working_dir,
                                                   ctx->prog_entry_dir, ctx->module_paths, ctx->num_module_paths, &ctx->tmp_mem);

            // Check if included file's path exists somewhere.
            if (ret == NIB_PATH_INV_PATH) {
                report_error(&ctx->errors, stmt->range, "Invalid include file path \"%s\"", stmt_include->file_pathname->str);
                return false;
            }

            // Check for .nib extension.
            if (ret == NIB_PATH_INV_EXT) {
                report_error(&ctx->errors, stmt->range, "Included file \"%s\" does not end in `.%s`", stmt_include->file_pathname->str,
                             nib_ext);
                return false;
            }

            assert(ret == NIB_PATH_OK);

            // Check that the include file is not the same as the current file.
            if (cstr_ncmp(file_ospath.str, include_ospath.str, path_len(&include_ospath)) == 0) {
                report_error(&ctx->errors, stmt->range, "Cyclic file inclusion detected at file `%s`. Cannot include self.",
                             file_ospath.str);
                return false;
            }

            // Check if the file we're trying to include is in the seen_includes table.
            // If yes, fail. Otherwise, add.
            u64 key = hash_bytes(include_ospath.str, path_len(&include_ospath), FNV_INIT);
            CachedInclude* cached_include = NULL;
            bool seen = false;

            // Look to see if this include file has already been seen.
            {
                u64* pval = hmap_get(seen_includes, key);

                if (pval) {
                    CachedInclude* cached = (void*)*pval;

                    for (CachedInclude* it = cached; it; it = it->next) {
                        if ((it->len == path_len(&include_ospath)) &&
                            (cstr_ncmp(it->str, include_ospath.str, path_len(&include_ospath)) == 0)) {
                            seen = true;
                            cached_include = it;
                            break;
                        }
                    }
                }
            }

            if (seen) {
                report_error(&ctx->errors, stmt->range, "Cyclic file inclusion detected.\nFile `%s` was first included by `%s`",
                             include_ospath.str, cached_include->includer_ospath->str);
                return false;
            }

            // Add to seen_includes
            {
                CachedInclude* new_cached_include =
                    mem_allocate(&ctx->tmp_mem, offsetof(CachedInclude, str) + path_len(&include_ospath) + 1, DEFAULT_ALIGN, true);

                if (!new_cached_include) {
                    NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
                    return false;
                }

                new_cached_include->next = cached_include;
                new_cached_include->len = path_len(&include_ospath);
                new_cached_include->includer_ospath = &file_ospath;

                memcpy(new_cached_include->str, include_ospath.str, path_len(&include_ospath));
                new_cached_include->str[path_len(&include_ospath)] = '\0';

                hmap_put(seen_includes, key, (uintptr_t)new_cached_include);
            }

            StringView included_code;

            Slurp_File_Err err = slurp_file(&included_code, ctx->gen_mem, include_ospath.str);
            if (err != SLURP_FILE_OK) {
                print_slurp_file_err(include_ospath.str, err);
                return false;
            }

            if (!parse_code_recursive(ctx, mod, include_ospath.str, path_len(&include_ospath), included_code.str, included_code.len,
                                      seen_includes, include_depth + 1)) {
                return false;
            }
        }
        else if (stmt->kind == CST_StmtImport) {
            StmtImport* stmt_import = (StmtImport*)stmt;

            // NOTE: A namespaced import only really imports one new symbol into a module.
            // NOTE: TODO: This is inaccurate for greedy imports (import all symbols raw)
            size_t num_imports = stmt_import->mod_namespace ? 1 : stmt_import->num_imports;

            mod->num_imports += num_imports;
            list_add_last(&mod->import_stmts, &stmt->lnode);
        }
        else if (stmt->kind == CST_StmtExport) {
            StmtExport* stmt_export = (StmtExport*)stmt;

            mod->num_exports += stmt_export->num_exports;
            list_add_last(&mod->export_stmts, &stmt->lnode);
        }
        else {
            if (stmt->kind == CST_StmtDecl) {
                StmtDecl* stmt_decl = (StmtDecl*)stmt;

                mod->num_decls += 1;
                if (stmt_decl->decl->flags & DECL_IS_EXPORTED)
                    mod->num_exports += 1;
            }

            list_add_last(&mod->stmts, &stmt->lnode);
        }

#ifdef NIBBLE_PRINT_IRS
        ftprint_out("%s\n", ftprint_stmt(ctx->gen_mem, stmt));
#endif
    }

#ifdef NIBBLE_PRINT_IRS
    ftprint_out("\n");
#endif

    return true;
}

static bool parse_code(NibbleCtx* ctx, Module* mod, const char* code, size_t code_len)
{
    AllocatorState tmp_state = allocator_get_state(&ctx->tmp_mem);
    HMap seen_includes = hmap(3, &ctx->tmp_mem);

    mod->range.start = get_curr_src_pos(&ctx->src_files);

    bool ret = parse_code_recursive(ctx, mod, mod->abs_path->str, mod->abs_path->len, code, code_len, &seen_includes, 0);

    mod->range.end = get_curr_src_pos(&ctx->src_files);

    allocator_restore_state(tmp_state);

    return ret;
}

static bool parse_module(NibbleCtx* ctx, Module* mod);

static Module* parse_import_module(NibbleCtx* ctx, const char* abs_path, size_t path_len)
{
    StrLit* abs_path_lit = intern_str_lit(&ctx->str_lit_map, abs_path, path_len);
    Module* mod = get_module(&ctx->mod_map, abs_path_lit);

    if (!mod) {
        mod = add_module(&ctx->mod_map, abs_path_lit);

        if (!parse_module(ctx, mod)) {
            return NULL;
        }
    }

    return mod;
}

bool import_builtin_syms(NibbleCtx* ctx, Module* mod)
{
    Module* src_mod = ctx->builtin_mod;
    List* head = &src_mod->scope.sym_list;
    List* it = head->next;

    while (it != head) {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (!module_add_global_sym(mod, sym->name, sym, &ctx->errors)) {
            return false;
        }

        it = it->next;
    }

    return true;
}

static bool parse_module(NibbleCtx* ctx, Module* mod)
{
    print_info(ctx, "Parsing module %s ...", mod->abs_path->str);

    mod->is_parsing = true;

    AllocatorState mem_state = allocator_get_state(&ctx->tmp_mem);

    Path mod_ospath = path_create(&ctx->tmp_mem, mod->abs_path->str, mod->abs_path->len);

    // Parse the code text
    StringView code;

    Slurp_File_Err err = slurp_file(&code, ctx->gen_mem, mod_ospath.str);
    if (err != SLURP_FILE_OK) {
        print_slurp_file_err(mod_ospath.str, err);
        return false;
    }

    if (!parse_code(ctx, mod, code.str, code.len)) {
        return false;
    }

    module_init_tables(mod, &ctx->ast_mem, ctx->num_builtins);

    // Import all builtin symbols.
    if (!import_builtin_syms(ctx, mod)) {
        return false;
    }

    // Install unresolved decls into the module's symbol table, and install exported decls into
    // the module's export table.
    if (!install_module_decls(&ctx->ast_mem, mod, &ctx->errors)) {
        return false;
    }

    // Process imports.
    {
        List* head = &mod->import_stmts;
        List* it = head->next;

        while (it != head) {
            Stmt* stmt = list_entry(it, Stmt, lnode);

            assert(stmt->kind == CST_StmtImport);
            StmtImport* simport = (StmtImport*)stmt;

            Path import_mod_ospath;
            NibblePathErr ret_err = get_import_abspath(&import_mod_ospath, simport->mod_pathname, &mod_ospath, ctx->working_dir,
                                                       ctx->prog_entry_dir, ctx->module_paths, ctx->num_module_paths, &ctx->tmp_mem);

            // Check if imported module's path exists somewhere.
            if (ret_err == NIB_PATH_INV_PATH) {
                report_error(&ctx->errors, stmt->range, "Invalid module import path \"%s\"", simport->mod_pathname->str);
                return false;
            }

            // Check for .nib extension.
            if (ret_err == NIB_PATH_INV_EXT) {
                report_error(&ctx->errors, stmt->range, "Imported module file \"%s\" does not end in `.%s`",
                             simport->mod_pathname->str, nib_ext);
                return false;
            }

            assert(ret_err == NIB_PATH_OK);

            //
            // Parse import module
            //

            Module* import_mod = parse_import_module(ctx, import_mod_ospath.str, path_len(&import_mod_ospath));

            if (!import_mod) {
                return false;
            }

            if (import_mod->is_parsing) {
                report_error(&ctx->errors, stmt->range, "Cyclic import \"%s\" detected", simport->mod_pathname->str);
                return false;
            }

            bool have_import_syms = !list_empty(&simport->import_syms);
            bool have_import_ns = simport->mod_namespace != NULL;

            if (have_import_ns) {
                Symbol* import_mod_sym = new_symbol_mod(&ctx->ast_mem, simport, import_mod, mod);

                if (!module_add_global_sym(mod, import_mod_sym->name, import_mod_sym, &ctx->errors)) {
                    return false;
                }
            }
            else if (have_import_syms) {
                if (!import_mod_syms(mod, import_mod, simport, &ctx->errors))
                    return false;
            }
            else {
                if (!import_all_mod_syms(mod, import_mod, &ctx->errors))
                    return false;
            }

            it = it->next;
        }
    }

    // Process exports
    {
        List* head = &mod->export_stmts;
        List* it = head->next;

        while (it != head) {
            Stmt* stmt = list_entry(it, Stmt, lnode);

            assert(stmt->kind == CST_StmtExport);
            StmtExport* sexport = (StmtExport*)stmt;

            List* h = &sexport->export_syms;

            for (List* i = h->next; i != h; i = i->next) {
                ExportSymbol* esym = list_entry(i, ExportSymbol, lnode);

                // Lookup the symbol we're trying to export.
                Symbol* sym = NULL;
                {
                    List* e_head = &esym->ns_ident.idents;
                    List* e_it = e_head->next;

                    IdentNode* inode = list_entry(e_it, IdentNode, lnode);
                    sym = lookup_scope_symbol(&mod->scope, inode->ident);
                    e_it = e_it->next;

                    // Keep looking through module namespaces.
                    while (sym && (e_it != e_head)) {
                        if (sym->kind != SYMBOL_MODULE) {
                            report_error(&ctx->errors, esym->range, "Namespace `%s` in export statement is not a module",
                                         inode->ident->str);
                            return false;
                        }

                        inode = list_entry(e_it, IdentNode, lnode);

                        StmtImport* stmt = (StmtImport*)sym->as_mod.stmt;
                        Identifier* sym_name = get_import_sym_name(stmt, inode->ident);

                        sym = module_get_export_sym(sym->as_mod.mod, sym_name);
                        e_it = e_it->next;
                    }

                    if (!sym) {
                        report_error(&ctx->errors, esym->range, "Unknown export symbol `%s`", inode->ident->str);
                        return false;
                    }
                }

                // Namespaced symbols must be renamed. Ex: export {MyMod::foo as foo};
                if ((esym->ns_ident.num_idents > 1) && !esym->rename) {
                    report_error(&ctx->errors, esym->range, "Exported symbol `%s` must be renamed due to module namespace",
                                 ftprint_ns_ident(&ctx->tmp_mem, &esym->ns_ident));
                    return false;
                }

                // Prevent users from exporting builtin symbols.
                if (sym->home == ctx->builtin_mod) {
                    report_error(&ctx->errors, esym->range, "Cannot export builtin symbol `%s`",
                                 ftprint_ns_ident(&ctx->tmp_mem, &esym->ns_ident));
                    return false;
                }

                // Add symbol to the module's export table
                Identifier* exp_name = esym->rename;

                if (!esym->rename) {
                    IdentNode* last_inode = list_entry(esym->ns_ident.idents.prev, IdentNode, lnode);
                    exp_name = last_inode->ident;
                }

                if (!module_add_export_sym(mod, exp_name, sym)) {
                    report_error(&ctx->errors, esym->range, "Conflicting export symbol name `%s`", exp_name->str);
                    return false;
                }
            }

            it = it->next;
        }
    }

    allocator_restore_state(mem_state);
    mod->is_parsing = false; // IMPORTANT: This should be only successful exit point for this procedure.
    return true;
}

bool nibble_compile(NibbleCtx* nib_ctx, const Path* main_path, const Path* out_path)
{
    AllocatorState mem_state = allocator_get_state(&nib_ctx->tmp_mem);

    print_info(nib_ctx, "Working directory: %s", nib_ctx->working_dir->str);
    print_info(nib_ctx, "Program entry directory: %s", nib_ctx->prog_entry_dir->str);

    // Builtin module
    static const char builtin_mod_name[] = "nibble_builtin";
    Module* builtin_mod =
        add_module(&nib_ctx->mod_map, intern_str_lit(&nib_ctx->str_lit_map, builtin_mod_name, sizeof(builtin_mod_name) - 1));
    nib_ctx->builtin_mod = builtin_mod;

    // Main module
    Module* main_mod = add_module(&nib_ctx->mod_map, intern_str_lit(&nib_ctx->str_lit_map, main_path->str, path_len(main_path)));
    nib_ctx->main_mod = main_mod;

    //////////////////////////////////////////
    //                Parse
    //////////////////////////////////////////
    const size_t num_builtin_types = ARRAY_LEN(builtin_types);
    const size_t builtin_code_len = cstr_len(builtin_code);
    bool parse_ok = parse_code(nib_ctx, builtin_mod, builtin_code, builtin_code_len);

    if (!parse_ok) {
        print_errors(nib_ctx);
        return false;
    }

    nib_ctx->num_builtins = builtin_mod->num_decls + num_builtin_types;
    builtin_mod->scope.sym_table = hmap(calc_hmap_size(nib_ctx->num_builtins << 1), &nib_ctx->ast_mem);

    init_builtin_syms(&nib_ctx->ast_mem, nib_ctx->builtin_mod, &nib_ctx->ident_map);

    if (!install_module_decls(&nib_ctx->ast_mem, builtin_mod, &nib_ctx->errors)) {
        print_errors(nib_ctx);
        return false;
    }

    // Parse main module.
    if (!parse_module(nib_ctx, main_mod)) {
        print_errors(nib_ctx);
        return false;
    }

    // Look for main to have been parsed and installed as an unresolved proc symbol.
    Symbol* main_sym = lookup_symbol(&main_mod->scope, main_proc_ident);

    if (!main_sym) {
        ProgPos main_start = main_mod->range.start;
        ProgRange err_range = {.start = main_start, .end = main_start};
        report_error(&nib_ctx->errors, err_range, "Program entry file must define a main() procedure.");
        print_errors(nib_ctx);
        return false;
    }

    if (main_sym->kind != SYMBOL_PROC) {
        report_error(&nib_ctx->errors, main_sym->decl->range, "Identifier `%s` must be a procedure, but found a %s.",
                     main_proc_ident->str, sym_kind_names[main_sym->kind]);
        print_errors(nib_ctx);
        return false;
    }

    //////////////////////////////////////////
    //          Resolve/Typecheck
    //////////////////////////////////////////
    print_info(nib_ctx, "Resolving/type-checking ...");

    Resolver resolver = {.ctx = nib_ctx};

    if (!resolve_module(&resolver, main_mod)) {
        print_errors(nib_ctx);
        return false;
    }

    if (!resolve_reachable_sym_defs(&resolver)) {
        print_errors(nib_ctx);
        return false;
    }

    // Ensure main has the expected type signature.
    Type* main_type = main_sym->type;
    assert(main_type->kind == TYPE_PROC);
    Type* main_ret_type = main_type->as_proc.ret;

    if (main_ret_type != builtin_types[BUILTIN_TYPE_INT].type) {
        DeclProc* main_decl = (DeclProc*)main_sym->decl;
        ProgRange err_range = main_decl->ret ? main_decl->ret->range : main_decl->super.range;

        report_error(&nib_ctx->errors, err_range, "Main procedure must return an `int` (`%s`) type, but found `%s`.",
                     type_name(builtin_types[BUILTIN_TYPE_INT].type), type_name(main_ret_type));
        print_errors(nib_ctx);
        return false;
    }

    size_t main_num_params = main_type->as_proc.num_params;

    // Check that params are either main(argc: int) or main(argc: int, argv: ^^char)
    if (main_num_params > 0) {
        DeclProc* main_decl = (DeclProc*)main_sym->decl;
        Type** param_types = main_type->as_proc.params;

        if (param_types[0] != builtin_types[BUILTIN_TYPE_INT].type) {
            DeclVar* param = (DeclVar*)list_entry(main_decl->params.next, Decl, lnode);

            report_error(&nib_ctx->errors, param->typespec->range,
                         "Main procedure's first paramater must be an `int` (`%s`) type, but found `%s`.",
                         type_name(builtin_types[BUILTIN_TYPE_INT].type), type_name(param_types[0]));
            print_errors(nib_ctx);
            return false;
        }

        if ((main_num_params == 2) && (param_types[1] != type_ptr_ptr_char)) {
            DeclVar* param = (DeclVar*)list_entry(main_decl->params.next->next, Decl, lnode);

            report_error(&nib_ctx->errors, param->typespec->range,
                         "Main procedure's second paramater must be a `^^char` type, but found `%s`.", type_name(param_types[1]));
            print_errors(nib_ctx);
            return false;
        }
    }

    //////////////////////////////////////////
    //          Gen IR bytecode
    //////////////////////////////////////////
    print_info(nib_ctx, "Generating IR ...");
    IR_gen_bytecode(&nib_ctx->ast_mem, &nib_ctx->tmp_mem, &nib_ctx->vars, &nib_ctx->procs, &nib_ctx->str_lits, &nib_ctx->float_lits,
                    &nib_ctx->type_cache, &nib_ctx->float_lit_map);

    //////////////////////////////////////////
    //          Gen output
    //////////////////////////////////////////
    assert(nib_ctx->target_arch == ARCH_X64); // TODO: Support other architectures
    Path obj_fname = path_createf(&nib_ctx->tmp_mem, "%.*s.o", path_len(out_path), out_path->str);

    x64_init_target(nib_ctx->target_os);

    if (nib_ctx->gen_asm) {
        Path nasm_fname = path_createf(&nib_ctx->tmp_mem, "%.*s.s", path_len(out_path), out_path->str);

        print_info(nib_ctx, "Generating NASM assembly output: %s ...", nasm_fname.str);
        x64_gen_nasm(nib_ctx->gen_mem, &nib_ctx->tmp_mem, &nib_ctx->vars, &nib_ctx->procs, &nib_ctx->str_lits,
                     &nib_ctx->float_lits, &nib_ctx->foreign_procs, nasm_fname.str);

        ftprint_out("[Example NASM command]: nasm -f elf64 %s -o %s\n", nasm_fname.str, obj_fname.str);
    }
    else {
        print_info(nib_ctx, "Generating ELF object file: %s ...", obj_fname.str);
        x64_gen_elf(nib_ctx->gen_mem, &nib_ctx->tmp_mem, &nib_ctx->vars, &nib_ctx->procs, main_sym, &nib_ctx->str_lits,
                    &nib_ctx->float_lits, &nib_ctx->foreign_procs, obj_fname.str);
    }

    //////////////////////////////////////////
    //          Run linker
    //////////////////////////////////////////
    ExecCmd ld_cmd = get_linker_cmd(&nib_ctx->tmp_mem, &nib_ctx->foreign_libs, nib_ctx->lib_paths, nib_ctx->num_lib_paths,
                                    nib_ctx->working_dir, obj_fname.str, out_path->str);

    if (!ld_cmd.argc) {
        return false;
    }

    if (nib_ctx->gen_asm) {
        Array(char) ld_cmd_str = cmd_to_str(&nib_ctx->tmp_mem, &ld_cmd);
        ftprint_out("[Example linker command]: %s\n", ld_cmd_str);
    }
    else if (run_cmd(&nib_ctx->tmp_mem, &ld_cmd, nib_ctx->silent) != 0) {
        ftprint_err("[ERROR]: Linker command failed\n");
        return false;
    }

    allocator_restore_state(mem_state);
    return true;
}

ForeignLib* nibble_add_foreign_lib(NibbleCtx* nib_ctx, const StrLit* foreign_lib_name)
{
    ForeignLib* lib = hmap_get_obj(&nib_ctx->foreign_lib_map, PTR_UINT(foreign_lib_name));

    if (!lib) {
        ForeignLibKind kind = FOREIGN_LIB_INVALID;
        const char* lib_ext = path_ext_ptr(foreign_lib_name->str, foreign_lib_name->len);

        if (cstr_cmp(lib_ext, shared_lib_ext) == 0) {
            kind = FOREIGN_LIB_SHARED;
        }
        else if (cstr_cmp(lib_ext, static_lib_ext) == 0) {
            kind = FOREIGN_LIB_STATIC;
        }
        else if (cstr_cmp(lib_ext, obj_file_ext) == 0) {
            kind = FOREIGN_LIB_OBJ;
        }
        else {
            return NULL;
        }

        lib = alloc_type(nib_ctx->gen_mem, ForeignLib, true);
        lib->name = foreign_lib_name;
        lib->kind = kind;

        hmap_put(&nib_ctx->foreign_lib_map, PTR_UINT(foreign_lib_name), PTR_UINT(lib));
        bucket_list_add_elem(&nib_ctx->foreign_libs, lib);
    }
    else {
        lib->ref_count += 1;
    }

    return lib;
}

void nibble_cleanup(NibbleCtx* nib_ctx)
{
#ifdef NIBBLE_PRINT_MEM_USAGE
    print_allocator_stats(nib_ctx->gen_mem, "GEN mem stats");
    print_allocator_stats(&nib_ctx->ast_mem, "AST mem stats");
    print_allocator_stats(&nib_ctx->tmp_mem, "TMP mem stats");
    ftprint_out("Ident map: len = %lu, cap = %lu, total_size (gen arean) = %lu\n", nib_ctx->ident_map.len, nib_ctx->ident_map.cap,
                nib_ctx->ident_map.cap * sizeof(HMapEntry));
    ftprint_out("StrLit map: len = %lu, cap = %lu, total_size (gen arean) = %lu\n", nib_ctx->str_lit_map.len, nib_ctx->str_lit_map.cap,
                nib_ctx->str_lit_map.cap * sizeof(HMapEntry));
    ftprint_out("FloatLit map: len = %lu, cap = %lu, total_size (gen arean) = %lu\n", nib_ctx->float_lit_map.len,
                nib_ctx->float_lit_map.cap, nib_ctx->float_lit_map.cap * sizeof(HMapEntry));
    ftprint_out("Module map: len = %lu, cap = %lu, total_size (ast arena) = %lu\n", nib_ctx->mod_map.len, nib_ctx->mod_map.cap,
                nib_ctx->mod_map.cap * sizeof(HMapEntry));
    ftprint_out("type_ptr cache: len = %lu, cap = %lu, total_size (ast arena) = %lu\n", nib_ctx->type_cache.ptrs.len,
                nib_ctx->type_cache.ptrs.cap, nib_ctx->type_cache.ptrs.cap * sizeof(HMapEntry));
    ftprint_out("type_array cache: len = %lu, cap = %lu, total_size (ast arena) = %lu\n", nib_ctx->type_cache.arrays.len,
                nib_ctx->type_cache.arrays.cap, nib_ctx->type_cache.arrays.cap * sizeof(HMapEntry));
    ftprint_out("type_proc cache: len = %lu, cap = %lu, total_size (ast arena) = %lu\n", nib_ctx->type_cache.procs.len,
                nib_ctx->type_cache.procs.cap, nib_ctx->type_cache.procs.cap * sizeof(HMapEntry));
    ftprint_out("type_slices cache: len = %lu, cap = %lu, total_size (ast arena) = %lu\n", nib_ctx->type_cache.slices.len,
                nib_ctx->type_cache.slices.cap, nib_ctx->type_cache.slices.cap * sizeof(HMapEntry));
    ftprint_out("type_structs cache: len = %lu, cap = %lu, total_size (ast arena) = %lu\n", nib_ctx->type_cache.structs.len,
                nib_ctx->type_cache.structs.cap, nib_ctx->type_cache.structs.cap * sizeof(HMapEntry));
    ftprint_out("type_unions cache: len = %lu, cap = %lu, total_size (ast arena) = %lu\n", nib_ctx->type_cache.unions.len,
                nib_ctx->type_cache.unions.cap, nib_ctx->type_cache.unions.cap * sizeof(HMapEntry));
#endif

    hmap_destroy(&nib_ctx->str_lit_map);
    hmap_destroy(&nib_ctx->float_lit_map);
    hmap_destroy(&nib_ctx->ident_map);
    hmap_destroy(&nib_ctx->mod_map);
    hmap_destroy(&nib_ctx->type_cache.ptrs);
    hmap_destroy(&nib_ctx->type_cache.procs);
    hmap_destroy(&nib_ctx->type_cache.arrays);
    hmap_destroy(&nib_ctx->type_cache.slices);
    hmap_destroy(&nib_ctx->type_cache.structs);
    hmap_destroy(&nib_ctx->type_cache.unions);
    allocator_destroy(&nib_ctx->tmp_mem);
    allocator_destroy(&nib_ctx->ast_mem);
}
