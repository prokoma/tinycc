#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

void print(int8_t c) {
    putchar(c);
}

void printnum(int64_t num) {
    printf("%" PRId64 "\n", num);
}

int8_t read() {
    return getchar();
}
