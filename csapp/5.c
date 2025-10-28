#include <stdio.h>
#include <stdlib.h>

// -- 5.17 -----------------
void *basic_memset(void *s, int c, size_t n) {
    unsigned char *schar = s;
    while (n--) {
        *schar++ = (unsigned char) c;
    }

    return s;
}

void *fast_memset(void *s, int c, size_t n) {
    const size_t K = sizeof (unsigned long);

    unsigned char *schar = s;
    while ((size_t) schar % K != 0) {
        *schar++ = (unsigned char) c;
        n--;
    }

    unsigned long *slong = (void *) schar;
    unsigned long clong = c & 0xff;
    clong = clong |
        (clong << 8) |
        (clong << 16) |
        (clong << 24) |
        (clong << 32) |
        (clong << 40) |
        (clong << 48) |
        (clong << 56);

    while (n >= K) {
        *slong++ = clong;
        n -= K;
    }

    schar = (void *) slong;
    while (n--) {
        *schar++ = (unsigned char) c;
    }

    return s;
}
// -------------------------

#define LEN 10000000000

int main() {
    char *buf = malloc(LEN * sizeof (char));

    basic_memset(buf, 69, LEN);

}
