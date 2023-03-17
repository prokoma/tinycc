#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

void print(char c) {
    putchar(c);
}

void printnum(int64_t num) {
    printf("%" PRId64 "\n", num);
}

char scan() {
    return getchar();
}
