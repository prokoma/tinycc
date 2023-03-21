void test1() {
    printnum(7 & 2);
// > 2

    printnum(6 & 3);
// > 2

    printnum(6 & 3 | 8);
// > 10

    // bitwise xor is not supported
}

void test2() {
    printnum(3 << 1);
// > 6

    printnum(~0 == -1);
// > 1

    printnum(3 >> 1);
// > 1
}

void test3() {
    char x = 64;

    // promotion
    printnum(x | 1024);
// > 1088

// this is undefined behaviour in C99
//    printnum(0 | (x << 1));
//// > -128
//
//    printnum(0 | (x << 2));
//// > 0

    printnum(cast<char>(x << 1));
// > -128

    printnum(cast<char>(x << 2));
// > 0

    printnum(cast<char>(x << cast<char>(2)));
// > 0
}

int main() {
    test1();
    test2();
    test3();

    return 0;
}