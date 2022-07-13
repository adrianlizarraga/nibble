// Implementation of the grisu double to string algorithm in the paper
// Printing Floating-Point Numbers Quickly and Accurately with Integers by Florian Loitsch
// https://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf

#define D_1_LOG2_10 0.30102999566398114 // 1/lg(10)

// x = f * (2^e)
typedef struct GrisuFP {
    u64 f; // 64-bit significand
    int e; // exponent (unbiased).
} GrisuFP;

typedef struct CachedPow10 {
    GrisuFP fp;
    int pow10_exp;
} CachedPow10;

// Generated by tool/gen_grisu_cached_powers.py
static const CachedPow10 pow10_cache[] = {
    {{0xFA8FD5A0081C0288, -1220}, -348},
    {{0xBAAEE17FA23EBF76, -1193}, -340},
    {{0x8B16FB203055AC76, -1166}, -332},
    {{0xCF42894A5DCE35EA, -1140}, -324},
    {{0x9A6BB0AA55653B2D, -1113}, -316},
    {{0xE61ACF033D1A45DF, -1087}, -308},
    {{0xAB70FE17C79AC6CA, -1060}, -300},
    {{0xFF77B1FCBEBCDC4F, -1034}, -292},
    {{0xBE5691EF416BD60C, -1007}, -284},
    {{0x8DD01FAD907FFC3C, -980}, -276},
    {{0xD3515C2831559A83, -954}, -268},
    {{0x9D71AC8FADA6C9B5, -927}, -260},
    {{0xEA9C227723EE8BCB, -901}, -252},
    {{0xAECC49914078536D, -874}, -244},
    {{0x823C12795DB6CE57, -847}, -236},
    {{0xC21094364DFB5637, -821}, -228},
    {{0x9096EA6F3848984F, -794}, -220},
    {{0xD77485CB25823AC7, -768}, -212},
    {{0xA086CFCD97BF97F4, -741}, -204},
    {{0xEF340A98172AACE5, -715}, -196},
    {{0xB23867FB2A35B28E, -688}, -188},
    {{0x84C8D4DFD2C63F3B, -661}, -180},
    {{0xC5DD44271AD3CDBA, -635}, -172},
    {{0x936B9FCEBB25C996, -608}, -164},
    {{0xDBAC6C247D62A584, -582}, -156},
    {{0xA3AB66580D5FDAF6, -555}, -148},
    {{0xF3E2F893DEC3F126, -529}, -140},
    {{0xB5B5ADA8AAFF80B8, -502}, -132},
    {{0x87625F056C7C4A8B, -475}, -124},
    {{0xC9BCFF6034C13053, -449}, -116},
    {{0x964E858C91BA2655, -422}, -108},
    {{0xDFF9772470297EBD, -396}, -100},
    {{0xA6DFBD9FB8E5B88F, -369}, -92},
    {{0xF8A95FCF88747D94, -343}, -84},
    {{0xB94470938FA89BCF, -316}, -76},
    {{0x8A08F0F8BF0F156B, -289}, -68},
    {{0xCDB02555653131B6, -263}, -60},
    {{0x993FE2C6D07B7FAC, -236}, -52},
    {{0xE45C10C42A2B3B06, -210}, -44},
    {{0xAA242499697392D3, -183}, -36},
    {{0xFD87B5F28300CA0E, -157}, -28},
    {{0xBCE5086492111AEB, -130}, -20},
    {{0x8CBCCC096F5088CC, -103}, -12},
    {{0xD1B71758E219652C, -77}, -4},
    {{0x9C40000000000000, -50}, 4},
    {{0xE8D4A51000000000, -24}, 12},
    {{0xAD78EBC5AC620000, 3}, 20},
    {{0x813F3978F8940984, 30}, 28},
    {{0xC097CE7BC90715B3, 56}, 36},
    {{0x8F7E32CE7BEA5C70, 83}, 44},
    {{0xD5D238A4ABE98068, 109}, 52},
    {{0x9F4F2726179A2245, 136}, 60},
    {{0xED63A231D4C4FB27, 162}, 68},
    {{0xB0DE65388CC8ADA8, 189}, 76},
    {{0x83C7088E1AAB65DB, 216}, 84},
    {{0xC45D1DF942711D9A, 242}, 92},
    {{0x924D692CA61BE758, 269}, 100},
    {{0xDA01EE641A708DEA, 295}, 108},
    {{0xA26DA3999AEF774A, 322}, 116},
    {{0xF209787BB47D6B85, 348}, 124},
    {{0xB454E4A179DD1877, 375}, 132},
    {{0x865B86925B9BC5C2, 402}, 140},
    {{0xC83553C5C8965D3D, 428}, 148},
    {{0x952AB45CFA97A0B3, 455}, 156},
    {{0xDE469FBD99A05FE3, 481}, 164},
    {{0xA59BC234DB398C25, 508}, 172},
    {{0xF6C69A72A3989F5C, 534}, 180},
    {{0xB7DCBF5354E9BECE, 561}, 188},
    {{0x88FCF317F22241E2, 588}, 196},
    {{0xCC20CE9BD35C78A5, 614}, 204},
    {{0x98165AF37B2153DF, 641}, 212},
    {{0xE2A0B5DC971F303A, 667}, 220},
    {{0xA8D9D1535CE3B396, 694}, 228},
    {{0xFB9B7CD9A4A7443C, 720}, 236},
    {{0xBB764C4CA7A44410, 747}, 244},
    {{0x8BAB8EEFB6409C1A, 774}, 252},
    {{0xD01FEF10A657842C, 800}, 260},
    {{0x9B10A4E5E9913129, 827}, 268},
    {{0xE7109BFBA19C0C9D, 853}, 276},
    {{0xAC2820D9623BF429, 880}, 284},
    {{0x80444B5E7AA7CF85, 907}, 292},
    {{0xBF21E44003ACDD2D, 933}, 300},
    {{0x8E679C2F5E44FF8F, 960}, 308},
    {{0xD433179D9C8CB841, 986}, 316},
    {{0x9E19DB92B4E31BA9, 1013}, 324},
    {{0xEB96BF6EBADF77D9, 1039}, 332},
    {{0xAF87023B9BF0EE6B, 1066}, 340},
};

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

typedef union F64Bits {
    double f;
    u64 i;
} F64Bits;

// Modified from StackOverflow answer by Stephen Canon: https://stackoverflow.com/a/12280075
double ceil(double x)
{
    const u64 sign_mask = 0x8000000000000000ULL;
    const u64 significand_mask = 0x000FFFFFFFFFFFFFULL;

    const F64Bits xrep = {.f = x};
    const F64Bits xabs = {.i = xrep.i & (~sign_mask)};
    const F64Bits threshold = {.f = 0x1.0p52};

    // All floating-point numbers larger than 2^52 are exact integers, so return x.
    // This also handles ceil(nan) = nan.
    if (xabs.i >= threshold.i) {
        return x;
    }

    const F64Bits one = {.f = 1.0};

    if (xabs.i < one.i) {

        // If x is in the range (1.0, 0.0], the result is a signed 0.0 with the sign of x.
        // This is generated by clearing everything but the sign bit of x.
        if (x <= 0.0) {
            return (F64Bits){.i = xrep.i & sign_mask}.f;
        }
        
        // Otherwise, x is in the range (0.0, 1.0), and the result is 1.0.
        return 1.0;
    }

    // The exponent of x is strictly in the range [0, 51], which means that
    // x contains both integral and fractional bits.
    const int exponent = xabs.i >> 52;
    const u64 fractional_mask = significand_mask >> exponent; // Mask that covers the fractional bits in significand.

    // If x is negative, just truncate off fractional bits.
    if (xrep.i & sign_mask) {
        return (F64Bits){.i = xrep.i & (~fractional_mask)}.f;
    }

    // X is positive, so force rounding away from zero.
    // Add fractional mask to x, and then truncate fractional bits. Overflow into the
    // exponent would still produce the desired result.
    return (F64Bits){.i = xrep.i + fractional_mask & ~fractional_mask}.f;
}

int grisu_k_comp(int e, int alpha, int gamma)
{
    return CEIL((alpha - e + 63) * D_1_LOG2_10);
}

