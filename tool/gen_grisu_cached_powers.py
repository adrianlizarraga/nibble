"""
Generates powers of 10 to be cached in our implementation of the grisu algorithm.

Paper: Printing Floating-Point Numbers Quickly and Accurately with Integers by Florian Loitsch
       https://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf

This file was adapated from the description/code in the following blog post by Victor Zverovich
https://www.zverovich.net/2019/02/11/formatting-floating-point-numbers.html
"""

MIN_POW_10 = -348
MAX_POW_10 = 340
POW_10_STEP = 8
NEG_EXP_OFFSET = 1600 # Large enough to bring 10^-348 out of decimal range.

def main():
    print("static const CachedPow10 pow10_cache[] = {")

    for p in range(MIN_POW_10, MAX_POW_10 + POW_10_STEP, POW_10_STEP):
        # If p >= 0, simply calculate 10^p.
        #
        # Otherwise, if p < 0, calculate (2^2000) / (10^-p). The exponent value will be corrected later by subtracting 2000.
        # Note that python integers, unlike floats, have unlimited precision. So, we need to divide by (10^-p) instead of multiplying
        # by 10^p to ensure that the calculation uses integers.
        pow10_val = 10 ** p if p >= 0 else (2 ** NEG_EXP_OFFSET) // (10 ** -p)
        e_offset = NEG_EXP_OFFSET if p < 0 else 0

        binary_str = "{:b}".format(int(pow10_val))

        # Take the most-significant 64 bits as the significand.
        # Note that rounding is done by adding 1 to the most-significant 65 bits, adding 1, and then dividing by 2.
        f = (int("{:0<{}}".format(binary_str[:65], 65), 2) + 1) // 2

        # For p >= 0, the exponent is equal to the number of bits we discarded: len(binary_str) - 64
        # For p < 0, the exponent must also be corrected by subtracting 2000.
        e = len(binary_str) - e_offset - 64
        print("    {{{{0x{:X}, {}}}, {}}},".format(f, e, p))

    print("};")

if __name__ == "__main__":
    main()
