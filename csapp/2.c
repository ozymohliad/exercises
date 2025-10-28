#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char * byte_pointer;

void show_bytes(byte_pointer ptr, size_t len) {
    for (int i = 0; i < len; i++) {
        printf("%.2x", ptr[i]);
    }

    printf("\n");
}

// -- 2.57 -----------------
void show_short(short i) {
    show_bytes((byte_pointer) &i, sizeof i);
}

void show_long(long i) {
    show_bytes((byte_pointer) &i, sizeof i);
}

void show_double(double i) {
    show_bytes((byte_pointer) &i, sizeof i);
}
// -------------------------


// --- 2.58 ----------------
int is_little_endian() {
    int x = 1;
    return * (byte_pointer) &x;
}
// -------------------------


// --- 2.59 ----------------
void ex259(int x, int y) {
    int res = (x & 0xff) | (y & ~0xff);
    printf("%.8x %.8x %.8x\n", x, y, res);
}
// -------------------------


// --- 2.60 ----------------
unsigned replace_byte(unsigned x, int i , unsigned char b) {
    return (x & ~(0xff << (i * 8))) | (b << (i * 8));
}
// -------------------------


// --- 2.61 ----------------
void ex261_1(int x) {
    int a = !!x;
    int b = !!~x;
    int c = !!(x & 0xff);
    int d = !!(0xff & ~(x >> ((sizeof(x) - 1) << 3)));

    printf("x = %#.8x\n", x);
    printf("%d %d %d %d\n", a, b, c, d);
}
// -------------------------


// --- 2.62 ----------------
int int_shifts_are_arithmetic() {
    int x = -1;
    return !!((x >> ((sizeof(int) << 3) - 1)) & 2);
}
// -------------------------


// --- 2.63 ----------------
unsigned srl(unsigned x, int k) {
    unsigned xsra = (int) x >> k;
    int k_comp = (sizeof(int) << 3) - k;
    int mask = (1 << k) - 1;
    return xsra & ~(mask << k_comp);
}

int sra(int x, int k) {
    int xsrl = (unsigned) x >> k;
    int k_comp = (sizeof(int) << 3) - k;
    int sign = !!(xsrl & (1 << (k_comp - 1)));
    int mask = (1 << k) - sign;
    return xsrl | (mask << k_comp);
}
// -------------------------


// --- 2.64 ----------------
int any_odd_one(unsigned x) {
    return !!(x & 0xaaaaaaaa);
}
// -------------------------


// --- 2.65 ----------------
int odd_ones(unsigned x) {
    x ^= (x >> 16);
    x ^= (x >> 8);
    x ^= (x >> 4);
    x ^= (x >> 2);
    x ^= (x >> 1);
    return x & 1;
}
// -------------------------


// --- 2.66 ----------------
int leftmost_one(unsigned x) {
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return x - (x >> 1);
}
// -------------------------


// --- 2.67 ----------------
int int_size_is_32() {
    int x = 1;
    x <<= 15;
    x <<= 15;
    x <<= 1;
    return x && !(x << 1);
}
// -------------------------


// --- 2.68 ----------------
int lower_one_mask(int n) {
    return (2 << (n - 1)) - 1;
}
// -------------------------


// --- 2.69 ----------------
unsigned rotate_left(unsigned x, int n) {
    int w = sizeof(int) << 3;
    int lbits = (x >> (w - n)) & ((1 << n) - 1);
    return (x << n) | lbits;
}
// -------------------------


// --- 2.70 ----------------
int fits_bits(int x, int n) {
    int pad = x >> (n - 1);
    return !pad | !~pad;
}
// -------------------------


// --- 2.71 ----------------
typedef unsigned packed_t ;

int  xbyte(packed_t word, int bytenum) {
    return ((int) word << (24 - (bytenum << 3)) >> 24);
}
// -------------------------


// --- 2.72 ----------------
void copy_int(int val, void *buf, int maxbytes) {
    if (maxbytes >= sizeof(val))
        memcpy(buf, (void *)&val, sizeof(val));
}
// -------------------------


// --- 2.73 ----------------
int saturating_add(int x, int y) {
    int w = (sizeof(int) << 3) - 1;
    int s = x + y;
    int is_overflow = (~(x ^ y) & (s ^ x)) >> w;
    int is_pos_over = is_overflow && ((s >> w) & 1);
    return (s & ~is_overflow) | (is_overflow & INT_MIN - is_pos_over);
}
// -------------------------


// --- 2.74 ----------------
int tsub_ok(int x, int y) {
    int diff = x - y;
    int w = (sizeof(int) << 3) - 1;
    return !((~(x ^ ~y) & (diff ^ x)) >> w);
}
// -------------------------


// --- 2.75 ----------------
int signed_high_prod(int x, int y) {
    return ((int64_t) x * y) >> (sizeof(int) << 3);
}

unsigned unsigned_high_prod(unsigned x, unsigned y) {
    int w = (sizeof(int) << 3);
    return signed_high_prod(x, y) + (x & ((int)y >> (w - 1))) + (y & ((int)x >> (w - 1)));
}
// -------------------------


// --- 2.76 ----------------
int umult_or_zero(unsigned x, unsigned y) {
    unsigned prod = x * y;
    int w = (sizeof(int) << 3) - 1;
    int is_over = (!x || prod/x == y) << w >> w;
    return prod & is_over;
}

void *calloc(size_t nmemb, size_t size) {
    size_t total_size = umult_or_zero(nmemb, size);
    if (!total_size) {
        return NULL;
    }

    void *ptr = malloc(total_size);
    memset(ptr, 0, total_size);
    return ptr;
}
// -------------------------


// --- 2.77 ----------------
void ex277(int x) {
    int a = x + (x << 4);
    int b = x - (x << 3);
    int c = (x << 6) - (x << 2);
    int d = (x << 4) - (x << 7);

    printf("17 * %d = %d (%d)\n", x, 17 * x, a);
    printf("-7 * %d = %d (%d)\n", x, -7 * x, b);
    printf("60 * %d = %d (%d)\n", x, 60 * x, c);
    printf("-112 * %d = %d (%d)\n", x, -112 * x, d);
}
// -------------------------


// --- 2.78 ----------------
int divide_power2(int x, int k) {
    int w = (sizeof(int) << 3) - 1;
    return (x + ((x >> w) & ((1 << k) - 1))) >> k;
}
// -------------------------


// --- 2.79 ----------------
int mul3div4(int x) {
    return divide_power2((x << 1) + x, 2);
}
// -------------------------


// --- 2.80 ----------------
int threefourths(int x) {
    int w = (sizeof(int) << 3) - 1;
    int bias = (~x & INT_MIN) && !!(x & 3);
    int onefourth = (x >> 2) + bias;
    return x - onefourth;
}
// -------------------------


// --- 2.81 ----------------
void ex281(int k, int j) {
    int a = ~((1 << k) - 1);
    int b = ((1 << k) - 1) << j;

    printf("k = %d; j = %d\n", k, j);
    printf("a = %.8x; b = %.8x\n", a, b);
}
// -------------------------


// --- 2.82 ----------------
// A: false for x = INT_MIN
// B: true
// C: true
// D: true
// E: true
// -------------------------


// --- 2.83 ----------------
double ex283(int y, int k) {
    return (double) y / ((1 << k) - 1);
}
// -------------------------


// --- 2.84 ----------------
unsigned f2u(float x) {
    return * (unsigned *) &x;
}

int float_le(float x, float y) {
    unsigned ux = f2u(x);
    unsigned uy = f2u(y);

    unsigned sx = ux >> 31;
    unsigned sy = uy >> 31;

    return !((ux << 1) | (uy << 1)) // both zero
        || (sx && !sy) // x is negative while y is positive
        || (sx == sy) && ((ux <= uy) ^ (sx & sy));
}
// -------------------------


// --- 2.85 ----------------
// A: E = 2       M = 1.11         f = 0.11         V = 7             Binary = 0 1000....001 11000...
// B: E = n       M = 2 - 2^(-n)   f = 1 - 2^(-n)   V = 2^(n+1) - 1   Binary = 0   n+Bias    11111...
// C: E = Bias-1  M = 1            f = 0            V = 2^(Bias-1)    Binary = 0 1111...1101 00000...
// -------------------------


// --- 2.86 ----------------
// k = 15, n = 63, Bias = 2^(k-1) - 1 = 16383
// Smallest pos denormalized: 2^(-n) * 2^(1-Bias)   = 2^(1-Bias-n)            = 2^(-16445)
// Smallest pos normalized:                         = 2^(1-Bias)              = 2^(-16382)
// Largest normalized:        (2 - 2^(-n)) * 2^Bias = 2^(Bias+1) - 2^(Bias-n) = 2^16384 - 2^16320
// -------------------------


// --- 2.87 ----------------
// k = 5, n = 10, Bias = 15
// ----------------------------------------------------------------------------
// Desc                    Hex   M             E    V               D
// ----------------------------------------------------------------------------
// -0                      8000  0             -14  -0              0
// Smallest value > 2      4001  1.0000000001  1    1025 * 2^(-9)   2.00195
// 512                     6000  1             9    512             512
// Largest denormalized    03FF  0.1111111111  -14  1023 * 2^(-24)  6.09756e-05
// -inf                    F600  -             -    -inf            -inf
// Num with hex 3BB0       3BB0  1.1110110000  -1   123 * 2^(-7)    0.96094
// ----------------------------------------------------------------------------


// --- 2.88 ----------------
// --------------------------------------------
// A bits       A value   B bits       B value
// --------------------------------------------
// 0 10110 011  176       0 1110 0110  176
// 1 00111 010  -5/1024   1 0000 0101  -5/1024
// 0 00000 111  7/131072  0 0000 0001  1/1024
// 1 11100 000  -8192     1 1111 0000  -inf
// 0 10111 100  384       0 1111 0000  inf
// --------------------------------------------


// --- 2.89 ----------------
// A: true
// B: false if x-y overflows or y = INT_MIN
// C: true
// D: false, product may not be exactly representable
// E: false in case of division by zero
// -------------------------


// --- 2.90 ----------------
float u2f(unsigned u) {
    return * (float *) &u;
}

float fpwr2(int x)
{
    /* Result exponent and fraction */
    unsigned exp, frac;
    unsigned u;
    if (x < -149) {
        /* Too small. Return 0.0 */
        exp = 0;
        frac = 0;
    } else if (x < -126) {
        /* Denormalized result */
        exp = 0;
        frac = 1 << (x + 149);
    } else if (x < 128) {
        /* Normalized result. */
        exp = x + 127;
        frac = 0;
    } else {
        /* Too big. Return +oo */
        exp = 255;
        frac = 0;
    }
    /* Pack exp and frac into 32 bits */
    u = exp << 23 | frac;
    /* Return as float */
    return u2f(u);
}
// -------------------------



// --- 2.91 ----------------
// 0x40490FDB -> 0 10000000 10010010000111111011011
// E = 1, M = 1.10010010000111111011011,
// A: Pi   = 11.0010010000111111011011
// B: 22/7 = 11.001001001001001...
// C:                   ^
// -------------------------


typedef unsigned float_bits;

float to_float(float_bits f) {
    return * (float *) &f;
}

float_bits from_float(float f) {
    return * (float_bits *) &f;
}

// --- 2.92 ----------------
float_bits float_negate(float_bits f) {
    int is_nan = !(~f & 0x7f800000) && (f & 0x007fffff);
    return f ^ (!is_nan << 31);
}
// -------------------------


// --- 2.93 ----------------
float_bits float_absval(float_bits f) {
    int is_nan = !(~f & 0x7f800000) && (f & 0x007fffff);
    return f & ((is_nan << 31) | INT_MAX);
}
// -------------------------


// --- 2.94 ----------------
float_bits float_twice(float_bits f) {
    int sign = f >> 31;
    int exp = 0xff & (f >> 23);
    int frac = f & 0x007fffff;

    if (exp == 255) {
        return f;
    } else if (exp == 0) {
        frac <<= 1;
    } else {
        exp += 1;
        frac = exp == 255 ? 0 : frac;
    }

    return (sign << 31) | (exp << 23) | frac;
}
// -------------------------


// --- 2.95 ----------------
float_bits float_half(float_bits f) {
    int sign = f >> 31;
    int exp = 0xff & (f >> 23);
    int frac = f & 0x007fffff;

    if (exp == 255) {
        return f;
    } else if (exp != 0) {
        exp -= 1;
        frac = exp ? frac : frac | (1 << 23);
    }

    if (exp == 0) {
        int l = frac & 1;
        frac >>= 1;
        frac = ((frac + l) & 1) ? frac : frac + l;
    }

    return (sign << 31) | (exp << 23) | frac;
}
// -------------------------


// --- 2.96 ----------------
int float_f2i(float_bits f) {
    int sign = f >> 31;
    int exp = 0xff & (f >> 23);
    int frac = f & 0x007fffff;

    if (exp == 255) {
        return INT_MIN;
    } else if (exp < 127) {
        return 0;
    } else {
        int e = exp - 127 - 23;
        int m = (frac | (1 << 23));
        return ((e > 7 ? INT_MIN : e >= 0 ? m << e : m >> (-e)) ^ (sign << 31 >> 31)) + sign;
    }
}
// -------------------------


// --- 2.97 ----------------
float_bits float_i2f(int i) {
    int sign = (unsigned) i >> 31;
    unsigned u = ((i >> 31) ^ i) + sign;

    int e = -1;
    for (unsigned x = u; x; e++, x >>= 1);

    if (e == -1) {
        return 0;
    }

    int exp = e + 127;

    if (e > 23) {
        int d = e - 23;
        int a = 1 << d;
        int remainder = (u & (a - 1)) - (1 << (d - 1));
        if (remainder >= 0) {
            u += (remainder == 0 && ((u + a) & a)) ? 0 : a;
            if (u & (1 << (e + 1))) {
                u >>= 1;
                exp += 1;
            }
        }
    }

    int frac = (u << 1 << (31 - e)) >> 9;

    return (sign << 31) | (exp << 23) | frac;
}
// -------------------------


int main() {
    // int x = -1073741823;
    // float f = to_float(float_i2f(x));
    // printf("%d -> %f %f\n", x, f, (float) x);

    for (int i = INT_MIN; i <= INT_MAX; i++) {
        float_bits f1 = float_i2f(i);
        float_bits f2 = from_float((float) i);
        if (f1 != f2) {
            printf("%d %.10f %.10f\n", i, to_float(f1), to_float(f2));
            return 1;
        }
    }
}
