#include "types.h"

static Type type_void_ = {.kind = TYPE_VOID};
static Type type_bool_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_BOOL};
static Type type_char_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_CHAR};
static Type type_schar_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_SCHAR};
static Type type_uchar_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_UCHAR};
static Type type_short_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_SHORT};
static Type type_ushort_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_USHORT};
static Type type_int_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_INT};
static Type type_uint_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_UINT};
static Type type_long_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_LONG};
static Type type_ulong_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_ULONG};
static Type type_llong_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_LLONG};
static Type type_ullong_ = {.kind = TYPE_INTEGRAL, .as_integral.kind = TYPE_ULLONG};
static Type type_float32_ = {.kind = TYPE_FLOAT, .as_float.kind = TYPE_FLOAT32};
static Type type_float64_ = {.kind = TYPE_FLOAT, .as_float.kind = TYPE_FLOAT64};

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
Type* type_float32 = &type_float32_;
Type* type_float64 = &type_float64_;
Type* type_ssize;
Type* type_usize;

size_t PTR_SIZE = 8;
size_t PTR_ALIGN = 8;

static const char* type_names[NUM_TYPE_KINDS] = {
    [TYPE_VOID] = "void",
    [TYPE_INTEGRAL] = "_integral_",
    [TYPE_FLOAT] = "_float_",
    [TYPE_ENUM] = "_enum_",
    [TYPE_PTR] = "_ptr_",
    [TYPE_PROC] = "_proc_",
    [TYPE_ARRAY] = "_array_",
    [TYPE_STRUCT] = "_struct_",
    [TYPE_UNION] = "_union_",
    [TYPE_CONST] = "_const_",
};

static const char* type_integral_names[NUM_TYPE_INTEGRAL_KINDS] = {
    [TYPE_INT] = "int",
    [TYPE_BOOL] = "bool",
    [TYPE_CHAR] = "char",
    [TYPE_SCHAR] = "schar",
    [TYPE_UCHAR] = "uchar",
    [TYPE_SHORT] = "short",
    [TYPE_USHORT] = "ushort",
    [TYPE_UINT] = "uint",
    [TYPE_LONG] = "long",
    [TYPE_ULONG] = "ulong",
    [TYPE_LLONG] = "llong",
    [TYPE_ULLONG] = "ullong"
};

static const char* type_float_names[NUM_TYPE_FLOAT_KINDS] = {
    [TYPE_FLOAT64] = "float64",
    [TYPE_FLOAT32] = "float32",
};

const char* type_name(Type* type)
{
    if (!type)
        return "null";

    switch (type->kind)
    {
        case TYPE_INTEGRAL:
            return type_integral_names[type->as_integral.kind];
        case TYPE_FLOAT:
            return type_float_names[type->as_float.kind];
        default:
            return type_names[type->kind];
    }
}

static Type* type_alloc(Allocator* allocator, TypeKind kind)
{
    Type* type = alloc_type(allocator, Type, true);
    type->kind = kind;

    return type;
}

Type* type_ptr(Allocator* allocator, Type* base)
{
    Type* type = type_alloc(allocator, TYPE_PTR);
    type->as_ptr.base = base;

    return type;
}

static size_t next_type_id = 1;

static void init_type(Type* type, size_t size, size_t align)
{
    type->id = next_type_id;
    type->size = size;
    type->align = align;

    next_type_id += 1;
}

static void init_integral_type(Type* type, size_t size, size_t align, bool is_signed, unsigned long long max)
{
    init_type(type, size, align);

    type->as_integral.is_signed = is_signed;
    type->as_integral.max = max;
}

void init_builtin_types(OS target_os, Arch target_arch)
{
    bool invalid_os_arch = false;

    init_type(type_void, 0, 0);
    init_integral_type(type_bool, 1, 1, false, 0xFF);
    init_integral_type(type_char, 1, 1, true, 0x7F);
    init_integral_type(type_schar, 1, 1, true, 0x7F);
    init_integral_type(type_uchar, 1, 1, false, 0xFF);
    init_integral_type(type_short, 2, 2, true, 0x7FFF);
    init_integral_type(type_ushort, 2, 2, false, 0xFFFF);
    init_integral_type(type_int, 4, 4, true, 0x7FFFFFFF);
    init_integral_type(type_uint, 4, 4, false, 0xFFFFFFFF);
    init_integral_type(type_llong, 8, 8, true, 0x7FFFFFFFFFFFFFFF);
    init_integral_type(type_ullong, 8, 8, false, 0xFFFFFFFFFFFFFFFF);
    init_type(type_float32, 4, 4);
    init_type(type_float64, 8, 8);

    switch (target_os)
    {
        case OS_LINUX:
            switch (target_arch)
            {
                case ARCH_X86:
                    init_integral_type(type_long, 4, 4, true, 0x7FFFFFFF);
                    init_integral_type(type_ulong, 4, 4, false, 0xFFFFFFFF);

                    PTR_SIZE = 4;
                    PTR_ALIGN = 4;
                    break;
                case ARCH_X64:
                    init_integral_type(type_long, 8, 8, true, 0x7FFFFFFFFFFFFFFF);
                    init_integral_type(type_ulong, 8, 8, false, 0xFFFFFFFFFFFFFFFF);

                    PTR_SIZE = 8;
                    PTR_ALIGN = 8;
                    break;
                default:
                    invalid_os_arch = true;
                    break;
            }
            break;
        case OS_WIN32:
            switch (target_arch)
            {
                case ARCH_X86:
                    init_integral_type(type_long, 4, 4, true, 0x7FFFFFFF);
                    init_integral_type(type_ulong, 4, 4, false, 0xFFFFFFFF);

                    PTR_SIZE = 4;
                    PTR_ALIGN = 4;
                    break;
                case ARCH_X64:
                    init_integral_type(type_long, 4, 4, true, 0x7FFFFFFF);
                    init_integral_type(type_ulong, 4, 4, false, 0xFFFFFFFF);

                    PTR_SIZE = 8;
                    PTR_ALIGN = 8;
                    break;
                default:
                    invalid_os_arch = true;
                    break;
            }
            break;
        case OS_OSX:
            switch (target_arch)
            {
                case ARCH_X64:
                    init_integral_type(type_long, 8, 8, true, 0x7FFFFFFFFFFFFFFF);
                    init_integral_type(type_ulong, 8, 8, false, 0xFFFFFFFFFFFFFFFF);

                    PTR_SIZE = 8;
                    PTR_ALIGN = 8;
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

    if (PTR_SIZE == 4)
    {
        type_ssize = type_int;
        type_usize = type_uint;

    }
    else
    {
        assert(PTR_SIZE == 8);
        type_ssize = type_llong;
        type_usize = type_ullong;
    }
}
