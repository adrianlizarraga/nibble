#ifndef NIBBLE_TYPES_H
#define NIBBLE_TYPES_H
#include <stddef.h>
#include <stdint.h>

#include "nibble.h"

#define TYPE_FLAG_IMMUTABLE  0x1

typedef enum TypeKind {
    TYPE_VOID,
    TYPE_INTEGRAL,
    TYPE_FLOAT,
    TYPE_ENUM,
    TYPE_PTR,
    TYPE_PROC,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_CONST,

    NUM_TYPE_KINDS,
} TypeKind;

typedef enum TypeIntegralKind {
    TYPE_INT,
    TYPE_BOOL,
    TYPE_CHAR,
    TYPE_SCHAR,
    TYPE_UCHAR,
    TYPE_SHORT,
    TYPE_USHORT,
    TYPE_UINT,
    TYPE_LONG,
    TYPE_ULONG,
    TYPE_LLONG,
    TYPE_ULLONG,
} TypeIntegralKind;

typedef enum TypeFloatKind {
    TYPE_FLOAT64,
    TYPE_FLOAT32,
} TypeFloatKind;

typedef enum TypeStatus {
    TYPE_STATUS_COMPLETE,
    TYPE_STATUS_INCOMPLETE,
    TYPE_STATUS_COMPLETING,
} TypeStatus;

typedef struct Type Type;

typedef struct TypeIntegral {
    TypeIntegralKind kind;
    bool is_signed;
    unsigned long long max;
} TypeIntegral;

typedef struct TypeFloat {
    TypeFloatKind kind;
} TypeFloat;

typedef struct TypeProc {
    size_t num_params;
    Type** params;
    Type* ret;
} TypeProc;

typedef struct TypeArray {
    Type* base;
    size_t len;
} TypeArray;

typedef struct TypePtr {
    Type* base;
} TypePtr;

typedef struct TypeConst {
    Type* base;
} TypeConst;

typedef struct TypeAggregateField {
    Type* type;
    size_t offset;
    const char* name;
} TypeAggregateField;

typedef struct TypeAggregate {
    size_t num_fields;
    TypeAggregateField* fields;
} TypeAggregate;

struct Type {
    TypeKind kind;
    TypeStatus status;
    int id;
    size_t size;
    size_t align;

    union {
        TypeIntegral as_integral;
        TypeFloat as_float;
        TypePtr as_ptr;
        TypeConst as_const;
        TypeProc as_proc;
        TypeArray as_array;
        TypeAggregate as_aggregate;
    };
};

extern Type* type_void;
extern Type* type_bool;
extern Type* type_char;
extern Type* type_schar;
extern Type* type_uchar;
extern Type* type_short;
extern Type* type_ushort;
extern Type* type_int;
extern Type* type_uint;
extern Type* type_long;
extern Type* type_ulong;
extern Type* type_llong;
extern Type* type_ullong;
extern Type* type_ssize;
extern Type* type_usize;
extern Type* type_float32;
extern Type* type_float64;

extern size_t PTR_SIZE;
extern size_t PTR_ALIGN;

void init_builtin_types(OS target_os, Arch target_arch);

#endif
