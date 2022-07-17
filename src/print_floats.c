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
#define D_1_LOG2_10 0.30102999566398114 // 1/lg(10)

// x = f * (2^e)
typedef struct GrisuFP {
    u64 f; // 64-bit significand
    int e; // exponent (unbiased).
} GrisuFP;

typedef union F64Bits {
    double f;
    u64 i;
} F64Bits;

typedef struct CachedPow10 {
    GrisuFP fp;
    int pow10_exp;
} CachedPow10;

// Generated by tool/gen_grisu_cached_powers.py
static const CachedPow10 pow10_cache[] = {
    {{0xFA8FD5A0081C0288ULL, -1220}, -348},
    {{0xBAAEE17FA23EBF76ULL, -1193}, -340},
    {{0x8B16FB203055AC76ULL, -1166}, -332},
    {{0xCF42894A5DCE35EAULL, -1140}, -324},
    {{0x9A6BB0AA55653B2DULL, -1113}, -316},
    {{0xE61ACF033D1A45DFULL, -1087}, -308},
    {{0xAB70FE17C79AC6CAULL, -1060}, -300},
    {{0xFF77B1FCBEBCDC4FULL, -1034}, -292},
    {{0xBE5691EF416BD60CULL, -1007}, -284},
    {{0x8DD01FAD907FFC3CULL, -980}, -276},
    {{0xD3515C2831559A83ULL, -954}, -268},
    {{0x9D71AC8FADA6C9B5ULL, -927}, -260},
    {{0xEA9C227723EE8BCBULL, -901}, -252},
    {{0xAECC49914078536DULL, -874}, -244},
    {{0x823C12795DB6CE57ULL, -847}, -236},
    {{0xC21094364DFB5637ULL, -821}, -228},
    {{0x9096EA6F3848984FULL, -794}, -220},
    {{0xD77485CB25823AC7ULL, -768}, -212},
    {{0xA086CFCD97BF97F4ULL, -741}, -204},
    {{0xEF340A98172AACE5ULL, -715}, -196},
    {{0xB23867FB2A35B28EULL, -688}, -188},
    {{0x84C8D4DFD2C63F3BULL, -661}, -180},
    {{0xC5DD44271AD3CDBAULL, -635}, -172},
    {{0x936B9FCEBB25C996ULL, -608}, -164},
    {{0xDBAC6C247D62A584ULL, -582}, -156},
    {{0xA3AB66580D5FDAF6ULL, -555}, -148},
    {{0xF3E2F893DEC3F126ULL, -529}, -140},
    {{0xB5B5ADA8AAFF80B8ULL, -502}, -132},
    {{0x87625F056C7C4A8BULL, -475}, -124},
    {{0xC9BCFF6034C13053ULL, -449}, -116},
    {{0x964E858C91BA2655ULL, -422}, -108},
    {{0xDFF9772470297EBDULL, -396}, -100},
    {{0xA6DFBD9FB8E5B88FULL, -369}, -92},
    {{0xF8A95FCF88747D94ULL, -343}, -84},
    {{0xB94470938FA89BCFULL, -316}, -76},
    {{0x8A08F0F8BF0F156BULL, -289}, -68},
    {{0xCDB02555653131B6ULL, -263}, -60},
    {{0x993FE2C6D07B7FACULL, -236}, -52},
    {{0xE45C10C42A2B3B06ULL, -210}, -44},
    {{0xAA242499697392D3ULL, -183}, -36},
    {{0xFD87B5F28300CA0EULL, -157}, -28},
    {{0xBCE5086492111AEBULL, -130}, -20},
    {{0x8CBCCC096F5088CCULL, -103}, -12},
    {{0xD1B71758E219652CULL, -77}, -4},
    {{0x9C40000000000000ULL, -50}, 4},
    {{0xE8D4A51000000000ULL, -24}, 12},
    {{0xAD78EBC5AC620000ULL, 3}, 20},
    {{0x813F3978F8940984ULL, 30}, 28},
    {{0xC097CE7BC90715B3ULL, 56}, 36},
    {{0x8F7E32CE7BEA5C70ULL, 83}, 44},
    {{0xD5D238A4ABE98068ULL, 109}, 52},
    {{0x9F4F2726179A2245ULL, 136}, 60},
    {{0xED63A231D4C4FB27ULL, 162}, 68},
    {{0xB0DE65388CC8ADA8ULL, 189}, 76},
    {{0x83C7088E1AAB65DBULL, 216}, 84},
    {{0xC45D1DF942711D9AULL, 242}, 92},
    {{0x924D692CA61BE758ULL, 269}, 100},
    {{0xDA01EE641A708DEAULL, 295}, 108},
    {{0xA26DA3999AEF774AULL, 322}, 116},
    {{0xF209787BB47D6B85ULL, 348}, 124},
    {{0xB454E4A179DD1877ULL, 375}, 132},
    {{0x865B86925B9BC5C2ULL, 402}, 140},
    {{0xC83553C5C8965D3DULL, 428}, 148},
    {{0x952AB45CFA97A0B3ULL, 455}, 156},
    {{0xDE469FBD99A05FE3ULL, 481}, 164},
    {{0xA59BC234DB398C25ULL, 508}, 172},
    {{0xF6C69A72A3989F5CULL, 534}, 180},
    {{0xB7DCBF5354E9BECEULL, 561}, 188},
    {{0x88FCF317F22241E2ULL, 588}, 196},
    {{0xCC20CE9BD35C78A5ULL, 614}, 204},
    {{0x98165AF37B2153DFULL, 641}, 212},
    {{0xE2A0B5DC971F303AULL, 667}, 220},
    {{0xA8D9D1535CE3B396ULL, 694}, 228},
    {{0xFB9B7CD9A4A7443CULL, 720}, 236},
    {{0xBB764C4CA7A44410ULL, 747}, 244},
    {{0x8BAB8EEFB6409C1AULL, 774}, 252},
    {{0xD01FEF10A657842CULL, 800}, 260},
    {{0x9B10A4E5E9913129ULL, 827}, 268},
    {{0xE7109BFBA19C0C9DULL, 853}, 276},
    {{0xAC2820D9623BF429ULL, 880}, 284},
    {{0x80444B5E7AA7CF85ULL, 907}, 292},
    {{0xBF21E44003ACDD2DULL, 933}, 300},
    {{0x8E679C2F5E44FF8FULL, 960}, 308},
    {{0xD433179D9C8CB841ULL, 986}, 316},
    {{0x9E19DB92B4E31BA9ULL, 1013}, 324},
    {{0xEB96BF6EBADF77D9ULL, 1039}, 332},
    {{0xAF87023B9BF0EE6BULL, 1066}, 340},
};

// Modified from musl libc implementation (MIT license)
// http://git.etalabs.net/cgit/musl/tree/src/math/ceil.c
double ceil(double x)
{
    const F64Bits xbits = {.f = x};
    const int exp = (xbits.i >> 52) & 0x7FF;
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

int grisu_k_comp(int e, int alpha, int gamma)
{
    return ceil((alpha - e + 63) * D_1_LOG2_10);
}

GrisuFP grisu_cached_power(int k)
{
    const int min_pow_exp = pow10_cache[0].pow10_exp;
    const int pow_exp_step = 8;

    int index = (k - min_pow_exp + (pow_exp_step - 1)) / pow_exp_step;
    assert(index >= 0 && index < (int)ARRAY_LEN(pow10_cache));

    return pow10_cache[index].fp;
}

GrisuFP grisu_fp_norm(GrisuFP x)
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

GrisuFP grisu_fp_from_dbl(double x)
{
    GrisuFP fp;
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

// Only valid for GrisuFP values with equal exponents.
GrisuFP grisu_fp_sub(GrisuFP x, GrisuFP y)
{
    assert(x.e == y.e && x.f >= y.f);
    GrisuFP result = {.f = x.f - y.f, .e = x.e};
    return result;
}

// x mult y = round_up( (fx * fy) / (2^64) ) * 2^(ex + ey + 64)
GrisuFP grisu_fp_mult(GrisuFP x, GrisuFP y)
{
    GrisuFP result;
    const u64 mask32 = 0xFFFFFFFF;
    u64 a, b, c, d, ac, bc, ad, bd, tmp;

    a = x.f >> 32;
    b = x.f & mask32;

    c = y.f >> 32;
    d = y.f & mask32;

    ac = a * c;
    bc = b * c;
    ad = a * d;
    bd = b * d;

    tmp = (bd >> 32) + (ad & mask32) + (bc & mask32);
    tmp += 1U << 31; // round
    
    result.f = ac + (ad >> 32) + (bc >> 32) + (tmp >> 32);
    result.e = x.e + y.e + 64;

    return result;
}

#define TEN7 10000000
void grisu1_cut(GrisuFP fp, u32 parts[3])
{
    parts[2] = (fp.f % (TEN7 >> fp.e)) << fp.e;

    u64 tmp = fp.f / (TEN7 >> fp.e);

    parts[1] = tmp % TEN7;
    parts[2] = tmp / TEN7;
}

void grisu1(double v, char* buffer)
{
    u32 ps[3];

    int q = 64;
    int alpha = 0;
    int gamma = 3;

    GrisuFP w = grisu_fp_norm(grisu_fp_from_dbl(v));
    int mk = grisu_k_comp(w.e + q, alpha, gamma);

    GrisuFP c_mk = grisu_cached_power(mk);
    GrisuFP d = grisu_fp_mult(w, c_mk);

    grisu1_cut(d, ps);

    sprintf(buffer, "%u%07u%07ue%d", ps[0], ps[1], ps[2], -mk);
}

int main(void) {
    double x = 0.801;

    printf("ceil(%f) = %f\n", x, ceil(x));

    char buffer[128] = {0};
    double grisu1_x = 1.12345;
    grisu1(grisu1_x, buffer);

    printf("grisu1(%f) = %s\n", grisu1_x, buffer);

    return 0;
}

