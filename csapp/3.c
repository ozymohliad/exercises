#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <complex.h>

// -- 3.58 -----------------
long decode2(long x, long y, long z) {
    y -= z;
    x *= y;
    return (y << 63 >> 63) ^ x;
}
// -------------------------


// -- 3.59 ----------------- 
//
//   ((x - 2^64*x_s) * (y - 2^64*y_s)) mod 2^128 =
// = (x*y - 2^64*x*y_s - 2^64*y*x_s + 2^128*x_s*y_s) mod 2^128 =
// = (p + 2^64*(-x*y_s - y*x_s)) mod 2^128 =                    | let p = x*y
// = (p_l + 2^64*(p_h - x*y_s - y*x_s)) mod 2^128
//
// -- dest in %rdi, x in %rsi, y in %rdx
// store_prod:
//     movq  %rdx, %rax     -- y in %rax
//     cqto                 -- sign-extend y into %rdx
//     movq  %rsi, %rcx     -- x in %rcx
//     sarq  $63, %rcx      -- replicate x's sign bit in %rcx
//     imulq %rax, %rcx     -- (-y * x_s) in %rcx
//     imulq %rsi, %rdx     -- (-x * y_s) in %rdx
//     addq  %rdx, %rcx     -- -(y * x_s + x * y_s) in %rcx
//     mulq  %rsi           -- p_l in %rax, p_h in %rdx
//     addq  %rcx, %rdx     -- p_h - y * x_s - x * y_s in %rdx
//     movq  %rax, (%rdi)   --  store
//     movq  %rdx, 8(%rdi)  --  result
//     ret
// ------------------------- 


// -- 3.60 ----------------- 
long loop(long x, int n) {
    long result = 0;
    long mask;
    for (mask = 1; mask != 0; mask <<= n) {
        result |= x & mask;
    }
    return result;
}
// ------------------------- 


// -- 3.61 ----------------- 
long cread_alt(long *xp) {
    long fallback = 0;
    return *(xp ? xp : &fallback);
}
// ------------------------- 


// -- 3.62 ----------------- 
typedef enum {MODE_A, MODE_B, MODE_C, MODE_D, MODE_E} mode_t;

long switch3(long *p1, long *p2, mode_t action) {
    long result = 0;
    switch (action) {
        case MODE_A:
            result = *p2;
            *p2 = *p1;
            break;
        case MODE_B:
            result = *p1 + *p2;
            *p1 = result;
            break;
        case MODE_C:
            *p1 = 59;
            result = *p2;
        case MODE_D:
            *p1 = *p2;
        case MODE_E:
            result = 27;
            break;
        default:
            result = 12;
    }
    return result;
}
// ------------------------- 


// -- 3.63 ----------------- 
long switch_prob(long x, long n) {
    long result = x;
    switch (n) {
        case 60:
        case 62:
            result = 8 * x;
            break;
        case 63:
            result = x >> 3;
            break;
        case 64:
            x *= 15;
        case 65:
            x *= x;
        default:
            result = x + 75;
    }
    return result;
}
// ------------------------- 


// -- 3.64 ----------------- 
// #define R 7
// #define S 5
// #define T 13
// ------------------------- 


// -- 3.65 ----------------- 
// A[i][j] in %rdx
// A[j][i] in %rax
// M = 15
// ------------------------- 


// -- 3.66 ----------------- 
// #define NR(n) 3 * (n)
// #define NC(n) 4 * (n) + 1
// ------------------------- 


// -- 3.67 ----------------- 
// A. |        strA s          |   z    |--------------------------------|        strB r          |--------------------------|
//    ^ (%rsp)        24(%rsp) ^        ^ 32(%rsp)                       ^ 64(%rsp)               ^ 88(%rsp)       104(%rsp) ^
// B. eval passes &r to process in %rdi
// C. process accesses s as if it was passed as and argument on the stack
// D. using the pointer to r in %rdi
// E. ...|      u[0]      |      u[1]      |       q        |...
//       ^ 64(%rsp)       ^ 72(%rsp)       ^ 80(%rsp)       ^ 88(%rsp)
// F. Structures are passed to functions on the stack.
//    Structures are returned from functions as pointers
//    that must also be passed as a hidden argument to the function
// ------------------------- 


// -- 3.68 ----------------- 
// #define A 9
// #define B 5
// ------------------------- 


// -- 3.69 ----------------- 
// #define CNT 7
//
// typedef struct {
//     long idx;
//     long x[4];
// } a_struct;
// ------------------------- 


// -- 3.70 ----------------- 
// A. e1.p    0
//    e1.y    8
//    e2.x    0
//    e2.next 8
// B. 16
// C:
union ele {
    struct {
        long *p;
        long y;
    } e1;
    struct {
        long x;
        union ele *next;
    } e2;
};

void proc(union ele *up) {
    up->e2.x = *(up->e2.next->e1.p) - up->e2.next->e1.y;
}
// ------------------------- 


// -- 3.71 -----------------
void good_echo() {
    int size = 32;
    char buf[size];
    char *newline;

    do {
        char *s = fgets(buf, size, stdin);
        if (s == NULL) {
            break;
        }
        
        newline = strchr(s, '\n');
        if (newline != NULL) {
            *++newline = '\0';
        }
        printf("%s", s);
    } while (newline == NULL);
}
// -------------------------


// -- 3.72 -----------------
// A. s2 = s1 - 16 * floor((8*n + 30) / 16)
//       = s1 - 16 * floor((8*(n + 2) + 14) / 16)
//       = s1 - 16 * floor((4*(n + 2) + 7) / 8)
//       = s1 - 16 * ceil(4*(n + 2) / 8)
//       = s1 - 16 * ceil((n + 2) / 2)
//       = s1 - 16 * (ceil(n / 2) + 1)
//       = s1 - 16*ceil(n / 2) - 16
//
// B. p  = 16 * floor((s2 + 15) / 16)
//       = 16 * ceil(s2 / 16)
//       = 16 * ceil((s1 - 16*ceil(n / 2) - 16) / 16)
//       = 16 * (ceil(s1 / 16) - ceil(n / 2) - 1)
//       = 16*ceil(s1 / 16) - 16*ceil(n / 2) - 16
//
// C. e1 = s1 - p - 8n
//       = s1 - 16*ceil(s1 / 16) + 16*ceil(n / 2) - 8*n + 16
//       = s1 - 16*ceil(s1 / 16) + 8*(2*ceil(n / 2) - n) + 16
//           let s1 = 16*qs + rs (qs is int, 0 <= rs < 16)
//               n  = 2*qn + rn  (qn is int, 0 <= rn 1)
//       = 16*qs + rs - 16*ceil((16*qs + rs) / 16) + 8*(2*ceil((2*qn + rn) / 2) - 2*qn - rn) + 16
//       = 16*qs + rs - 16*qs - 16*ceil(rs / 16) + 8*(2*qn + 2*ceil(rn / 2) - 2*qn - rn) + 16
//       = rs - 16*ceil(rs / 16) + 8*(2*ceil(rn / 2) - rn) + 16
//
//      Max e1 when s1 is of the form 16*qs (rs = 0) and n is of the form 2*qn + 1 (rn = 1): 8 + 16 = 24
//      Min e1 when s1 if of the form 16*qs + 1 (rs = 1) and n is of the form 2*qn (rn = 0): 1 - 16 + 16 = 1
//
// D. s2 is aligned on 16 if s1 is. p is always aligned on 16
// -------------------------


// -- 3.73 and 3.74 --------
typedef enum {NEG, ZERO, POS, OTHER} range_t;

range_t find_range(float x) {
    int result;
    if (x < 0)
        result = NEG;
    else if (x == 0)
        result = ZERO;
    else if (x > 0)
        result = POS;
    else
        result = OTHER;
    return result;
}

range_t find_range1(float x) {
    range_t result;
    asm("vxorps %%xmm1, %%xmm1, %%xmm1\n\t"
        "vucomiss %%xmm1, %%xmm0\n\t"
        "jp .oth\n\t"
        "jb .neg\n\t"
        "je .zer\n\t"
        "ja .pos\n"
        ".neg:\n\tmovl $0, %[res]\n\tjmp .done\n"
        ".zer:\n\tmovl $1, %[res]\n\tjmp .done\n"
        ".pos:\n\tmovl $2, %[res]\n\tjmp .done\n"
        ".oth:\n\tmovl $3, %[res]\n\tjmp .done\n"
        ".done:"
        : [res] "=r" (result)
        :
        : "%xmm1", "%r8", "%r9", "%r10", "%r11");
    return result;
}

range_t find_range2(float x) {
    range_t result;
    asm("vxorps %%xmm1, %%xmm1, %%xmm1\n\t"
        "vucomiss %%xmm1, %%xmm0\n\t"
        "movl $0, %%r8d\n\t"
        "movl $1, %%r9d\n\t"
        "movl $2, %%r10d\n\t"
        "movl $3, %%r11d\n\t"
        "cmovb %%r8d, %[res]\n\t"
        "cmove %%r9d, %[res]\n\t"
        "cmova %%r10d, %[res]\n\t"
        "cmovp %%r11d, %[res]"
        : [res] "=r" (result)
        :
        : "%xmm1", "%r8", "%r9", "%r10", "%r11");
    return result;
}
// -------------------------


// -- 3.75 -----------------
// A. Complex numbers are passed in the same way as regular floats,
//    but they take up two xmm registers instead of one.
//    Real part in register %xmmX, imaginary part in %xmm(X+1)
// B. Real part in register %xmm0, imaginary part in %xmm1
// -------------------------


int main() {
    double complex x = 3.14 + 5 * I;
    printf("%f + i%f\n", x, 100.2);
}
