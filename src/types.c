#include "types.h"

static Type type_void_ = {.kind = TYPE_VOID};
static Type type_bool_ = {.kind = TYPE_BOOL};
static Type type_char_ = {.kind = TYPE_CHAR};
static Type type_schar_ = {.kind = TYPE_SCHAR};
static Type type_uchar_ = {.kind = TYPE_UCHAR};
static Type type_short_ = {.kind = TYPE_SHORT};
static Type type_ushort_ = {.kind = TYPE_USHORT};
static Type type_int_ = {.kind = TYPE_INT};
static Type type_uint_ = {.kind = TYPE_UINT};
static Type type_long_ = {.kind = TYPE_LONG};
static Type type_ulong_ = {.kind = TYPE_ULONG};
static Type type_llong_ = {.kind = TYPE_LLONG};
static Type type_ullong_ = {.kind = TYPE_ULLONG};
static Type type_float_ = {.kind = TYPE_FLOAT};
static Type type_double_ = {.kind = TYPE_DOUBLE};

Type* type_void = &type_void_;
Type* type_bool = &type_bool_;
Type* type_char = &type_char_;
Type* type_schar = &type_schar_;
Type* type_uchar = &type_uchar_;
Type* type_short = &type_short_;
Type* type_ushort = &type_ushort_;
Type* type_int = &type_int_;
Type* type_uint = &type_uint_;
Type* type_long = &type_long_;
Type* type_ulong = &type_ulong_;
Type* type_llong = &type_llong_;
Type* type_ullong = &type_ullong_;
Type* type_float = &type_float_;
Type* type_double = &type_double_;
Type* type_ssize;
Type* type_usize;

typedef struct TargetTypeMetrics {
    size_t size;
    size_t align;
    bool is_signed;
} TargetTypeMetrics;

static TargetTypeMetrics target_type_metrics[NUM_TYPE_KINDS];
static size_t next_type_id = 1;

void init_builtin_type(Type* type)
{
    type->id = next_type_id++;
    type->size = target_type_metrics[type->kind].size;
    type->align = target_type_metrics[type->kind].align;
}

void init_builtin_types(OS target_os, Arch target_arch)
{
    bool invalid_os_arch = false;

    target_type_metrics[TYPE_BOOL] = (TargetTypeMetrics){.size = 1, .align = 1};
    target_type_metrics[TYPE_CHAR] = (TargetTypeMetrics){.size = 1, .align = 1, .is_signed = true};
    target_type_metrics[TYPE_SCHAR] = (TargetTypeMetrics){.size = 1, .align = 1, .is_signed = true};
    target_type_metrics[TYPE_UCHAR] = (TargetTypeMetrics){.size = 1, .align = 1};
    target_type_metrics[TYPE_SHORT] = (TargetTypeMetrics){.size = 2, .align = 2, .is_signed = true};
    target_type_metrics[TYPE_USHORT] = (TargetTypeMetrics){.size = 2, .align = 2};
    target_type_metrics[TYPE_INT] = (TargetTypeMetrics){.size = 4, .align = 4, .is_signed = true};
    target_type_metrics[TYPE_UINT] = (TargetTypeMetrics){.size = 4, .align = 4};
    target_type_metrics[TYPE_LLONG] = (TargetTypeMetrics){.size = 8, .align = 8, .is_signed = true};
    target_type_metrics[TYPE_ULLONG] = (TargetTypeMetrics){.size = 8, .align = 8};

    switch (target_os) {
        case OS_LINUX:
            switch (target_arch) {
                case ARCH_X86:
                    target_type_metrics[TYPE_LONG] = (TargetTypeMetrics){.size = 4, .align = 4, .is_signed = true};
                    target_type_metrics[TYPE_ULONG] = (TargetTypeMetrics){.size = 4, .align = 4};
                    target_type_metrics[TYPE_PTR] = (TargetTypeMetrics){.size = 4, .align = 4};
                    break;
                case ARCH_X64:
                    target_type_metrics[TYPE_LONG] = (TargetTypeMetrics){.size = 8, .align = 8, .is_signed = true};
                    target_type_metrics[TYPE_ULONG] = (TargetTypeMetrics){.size = 8, .align = 8};
                    target_type_metrics[TYPE_PTR] = (TargetTypeMetrics){.size = 8, .align = 8};
                    break;
                default:
                    invalid_os_arch = true;
                    break;
            }
            break;
        case OS_WIN32:
            switch (target_arch) {
                case ARCH_X86:
                    target_type_metrics[TYPE_LONG] = (TargetTypeMetrics){.size = 4, .align = 4, .is_signed = true};
                    target_type_metrics[TYPE_ULONG] = (TargetTypeMetrics){.size = 4, .align = 4};
                    target_type_metrics[TYPE_PTR] = (TargetTypeMetrics){.size = 4, .align = 4};
                    break;
                case ARCH_X64:
                    target_type_metrics[TYPE_LONG] = (TargetTypeMetrics){.size = 4, .align = 4, .is_signed = true};
                    target_type_metrics[TYPE_ULONG] = (TargetTypeMetrics){.size = 4, .align = 4};
                    target_type_metrics[TYPE_PTR] = (TargetTypeMetrics){.size = 8, .align = 8};
                    break;
                default:
                    invalid_os_arch = true;
                    break;
            }
            break;
        case OS_OSX:
            switch (target_arch) {
                case ARCH_X64:
                    target_type_metrics[TYPE_LONG] = (TargetTypeMetrics){.size = 8, .align = 8, .is_signed = true};
                    target_type_metrics[TYPE_ULONG] = (TargetTypeMetrics){.size = 8, .align = 8};
                    target_type_metrics[TYPE_PTR] = (TargetTypeMetrics){.size = 8, .align = 8};
                    break;
                default:
                    invalid_os_arch = true;
                    break;
            }
            break;
        default:
            invalid_os_arch = true;
            break;
    }

    if (invalid_os_arch)
    {
        ftprint_err("Unsupported OS architecture: %s %s\n", os_names[target_os], arch_names[target_arch]);
        exit(1);
    }

    init_builtin_type(type_void);
    init_builtin_type(type_bool);
    init_builtin_type(type_char);
    init_builtin_type(type_schar);
    init_builtin_type(type_uchar);
    init_builtin_type(type_short);
    init_builtin_type(type_ushort);
    init_builtin_type(type_int);
    init_builtin_type(type_uint);
    init_builtin_type(type_long);
    init_builtin_type(type_ulong);
    init_builtin_type(type_llong);
    init_builtin_type(type_ullong);
    init_builtin_type(type_float);
    init_builtin_type(type_double);

    if (target_type_metrics[TYPE_PTR].size == 4)
    {
        type_ssize = type_int;
        type_usize = type_uint;
    }
    else
    {
        assert(target_type_metrics[TYPE_PTR].size == 8);
        type_ssize = type_llong;
        type_usize = type_ullong;
    }
}
