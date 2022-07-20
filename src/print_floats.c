// Implementation of the grisu double to string algorithm in the paper
// Printing Floating-Point Numbers Quickly and Accurately with Integers by Florian Loitsch
// https://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

typedef uint64_t u64;
typedef uint32_t u32;

#define ARRAY_LEN(a) (sizeof(a) / sizeof((a)[0]))
#define PRINTF_MAX_NUM_DIGITS 32
#define F64_SIGN_MASK 0x8000000000000000ULL
#define F64_EXP_MASK 0x7FF0000000000000ULL
#define F64_EXP_POS 52
#define F64_FRAC_MASK 0x000FFFFFFFFFFFFFULL


// x = f * (2^e)
typedef struct CustomFP {
    u64 f; // 64-bit significand
    int e; // exponent (unbiased).
} CustomFP;

void custom_fp_debug_print(const char* label, CustomFP x)
{
    printf("%s: 0x%016lX * 2^(%d)\n", label, x.f, x.e);
}

typedef union F64Bits {
    double f;
    u64 i;
} F64Bits;

// Modified from musl libc implementation (MIT license)
// http://git.etalabs.net/cgit/musl/tree/src/math/ceil.c
double ceil(double x)
{
    const F64Bits xbits = {.f = x};
    const int exp = (xbits.i & F64_EXP_MASK) >> F64_EXP_POS;
    const int exp_52 = 0x3FF + 52;

    // All floating-point numbers larger than 2^52 are exact integers, so return x.
    // This also handles 0, "NaN", and "inf".
    if (exp >= exp_52 || x == 0.0) {
        return x;
    }

    const int exp_neg_1 = 0x3FF - 1;
    const bool is_neg = xbits.i >> 63;

    // If |x| < 1, then negative numbers round to -0, and positive numbers round to 1.
    if (exp <= exp_neg_1) {
        return is_neg ? -0.0 : 1.0;
    }

    // The exponent is guaranteed to be in the range [0, 51] from this point forward.
    // Use addition with 2^52 to get the nearest integer neighbor.
    //
    // Examples:
    //     10.7 + 2^52 - 2^52 => 11.0
    //     10.2 + 2^52 - 2^52 => 10.0
    //     -10.1 - 2^52 + 2^52 => -10.0

    const double thresh_2p52 = 0x1.0p52;
    const double int_neighbor = is_neg ? (x - thresh_2p52 + thresh_2p52) : (x + thresh_2p52 - thresh_2p52);
    const double neighbor_diff = int_neighbor - x;

    if (neighbor_diff < 0.0) {
        return x + neighbor_diff + 1.0;
    }

    return x + neighbor_diff;
}

CustomFP custom_fp_norm(CustomFP x)
{
    const u64 last_bit_mask = 0x8000000000000000ULL;
    const u64 last_byte_mask = 0xFF00000000000000ULL;

    // Shift left until the most-significant byte has a 1.
    while (!(x.f & last_byte_mask)) {
        x.f = x.f << 8;
        x.e -= 8;
    }

    // Shift left until the most-significant bit is a 1 (i.e., normalized).
    while (!(x.f & last_bit_mask)) {
        x.f = x.f << 1;
        x.e -= 1;
    }

    return x;
}

CustomFP custom_fp_from_f64(double x)
{
    CustomFP fp;
    F64Bits bits = {.f = x};

    // Note that the exponent bias is traditionally 1023, but we want to treat the "fraction" as a non-fraction.
    // So, we add 52 (length of fraction bits).
    const int exp_bias = 1075;
    const int exp_pos = 52;
    const u64 exp_mask = 0x7FF0000000000000ULL;
    const u64 fraction_mask = 0x000FFFFFFFFFFFFFULL;
    const u64 implicit_one = 0x0010000000000000ULL;

    // Handle 0 (exp == 0, f == 0) and subnormals (exp == 0, f != 0)
    //
    // For subnormals, the double is (-1)^sign * 2^(1 - 1023) * 0.fraction
    // OR, (-1)^sign * 2^(1 - 1075) * fraction
    //
    // For zero, the same computation just works.
    if (!(bits.i & exp_mask)) {
        fp.f = bits.i & fraction_mask;
        fp.e = 1 - exp_bias;
    }
    // Normal doubles.
    // (-1)^sign * 2^(exp - 1023) * 1.fraction
    // OR,
    // (-1)^sign * 2^(exp - 1075) * (2^52 + fraction)
    else {
        int unbiased_exp = ((bits.i & exp_mask) >> exp_pos);
        fp.f = implicit_one + (bits.i & fraction_mask);
        fp.e = unbiased_exp - exp_bias;
    }

    return fp;
}

enum F64StringFlags {
    F64_STRING_IS_NEG = 1 << 0,
    F64_STRING_IS_INF = 1 << 1,
    F64_STRING_IS_NAN = 1 << 2,
};

typedef struct F64String {
    char digits[1024];
    int num_digits;
    int decimal_point; // location of the decimal point (left of corresponding digit index).
    int flags;
} F64String;

// Adapted from https://research.swtch.com/ftoa
void f64_to_str(F64String* dst, double f)
{
    dst->num_digits = 0;
    dst->decimal_point = 0;
    dst->flags = 0;

    F64Bits fbits = {.f = f};

    // Handle negative values.
    if (fbits.i >> 63) {
        fbits.i ^= F64_SIGN_MASK; // Make positive.
        dst->flags |= F64_STRING_IS_NEG;
    }

    // Handle zero
    if (!fbits.i) {
        dst->num_digits = 1;
        dst->digits[0] = '0';
        dst->decimal_point = 1;

        return;
    }

    int biased_exp = (fbits.i & F64_EXP_MASK) >> F64_EXP_POS;
    u64 fraction = (fbits.i & F64_FRAC_MASK);

    // Handle infinity: exponent = 0x7FF, fraction = 0.
    // Handle NaNs: exponent = 0x7FF, fraction != 0.
    if (biased_exp == 0x7FF) {
        dst->flags |= (fraction != 0 ? F64_STRING_IS_NAN : F64_STRING_IS_INF);
        return;
    }

    CustomFP fp = custom_fp_norm(custom_fp_from_f64(fbits.f));

    custom_fp_debug_print("fp", fp);

    // Convert significand to a string (itoa)
    {
        char tmp_buf[PRINTF_MAX_NUM_DIGITS];
        int len = 0;
        u64 value = fp.f;

        // Write digits into tmp_buf in reverse order.
        do {
            assert(len < PRINTF_MAX_NUM_DIGITS);
            tmp_buf[len++] = '0' + (char)(value % 10);
            value = value / 10;
        } while (value > 0);

        // Copy into dst buffer in the correct order.
        for (int i = len - 1; i >= 0; i--) {
            dst->digits[dst->num_digits++] = tmp_buf[i];
        }
    }

    // Multiply significand by 2 "e" times.
    int e = fp.e;

    for (; e > 0; e--) {
        bool add_digit = dst->digits[0] >= '5';
        char carry = 0;

        for (int i = dst->num_digits - 1; i >= 0; i--) {
            int x = carry + 2 * (dst->digits[i] - '0');

            carry = x / 10; // TODO: x >= 10
            dst->digits[i + add_digit] = (x % 10) + '0';
        }

        if (add_digit) {
            dst->digits[0] = '1';
            dst->num_digits += 1;
        }
    }

    dst->decimal_point = dst->num_digits;

    for (; e < 0; e++) {

        // If the last digit is odd, add a new digit for the .5
        if (dst->digits[dst->num_digits - 1] % 2 != 0) {
            dst->digits[dst->num_digits] = '0';
            dst->num_digits += 1;
        }

        int read_delta = 0;
        char prev_rem = 0;

        // Just like when dividing by hand, if the first (left-most) digit is less than 2,
        // then we have to consider the first two digits together.
        if (dst->digits[0] < '2') {
            read_delta = 1;
            prev_rem = dst->digits[0] - '0';
            dst->num_digits -= 1;
            dst->decimal_point -= 1;
        }

        // Divide by 2 (left to right).
        // 'prev_rem' is the remainder of the previous step.
        for (int i = 0; i < dst->num_digits; i++) {
            int x = (prev_rem * 10) + (dst->digits[i + read_delta] - '0');

            dst->digits[i] = (x / 2) + '0';
            prev_rem = x % 2;
        }
    }

    dst->digits[dst->num_digits] = '\0';
}

int main(void) {
    //F64Bits f = {.i = 0x1}; // 4.9406564584124654 × 10−324 (Min. subnormal positive double) ==> 0x1 as an int
    //F64Bits f = {.i = 0x7FF0000000000001ULL}; // +inf => 0x7FF0000000000000ULL
    //F64Bits f = {.i = 0xFFF0000000000001ULL}; // -inf => 0xFFF0000000000000ULL
    F64Bits f = {.i = 0x7FF0000000000001ULL}; // NaN => 0x7FFxxxxxxxxxxxxxULL where xx.. is >= 1
    F64String fstr = {0};

    f64_to_str(&fstr, f.f);
    char sign = (fstr.flags & F64_STRING_IS_NEG) ? '-' : '+';

    if (fstr.flags & F64_STRING_IS_INF) {
        printf("f64_to_str(%e) = %cinf\n", f.f, sign);
    }
    else if (fstr.flags & F64_STRING_IS_NAN) {
        printf("f64_to_str(%e) = nan\n", f.f);
    }
    else {
        printf("f64_to_str(%e) = %c%c.%se%+d\n", f.f, sign, fstr.digits[0], fstr.digits + 1, fstr.decimal_point - 1);
        printf("decimal_point = %d, num_digits = %d\n", fstr.decimal_point, fstr.num_digits);
    }

    return 0;
}

