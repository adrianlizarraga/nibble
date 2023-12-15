#ifndef NIBBLE_PRINT_FLOATS_H
#define NIBBLE_PRINT_FLOATS_H
#include "basics.h"

#define F64_SIGN_MASK 0x8000000000000000ULL
#define F64_EXP_MASK 0x7FF0000000000000ULL
#define F64_EXP_POS 52
#define F64_FRAC_MASK 0x000FFFFFFFFFFFFFULL

enum F64StringFlags {
    F64_STRING_IS_NEG = 1 << 0,
    F64_STRING_IS_INF = 1 << 1,
    F64_STRING_IS_NAN = 1 << 2,
};

typedef struct F64String {
    char digits[1024];
    u32 num_digits;
    int decimal_point; // location of the decimal point (left of corresponding digit index).
    u32 flags;
} F64String;

double ceil(double x);

// Adapted from https://research.swtch.com/ftoa
void f64_to_str(F64String* dst, double f);

u32 f64str_num_frac_digits(F64String* fstr);
u32 f64str_num_int_digits(F64String* fstr);
bool f64str_has_nonzero_digit(F64String* fstr, u32 round_digit);
void f64str_round(F64String* fstr, u32 precision);

#endif
