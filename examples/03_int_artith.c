void test1() {
    int x = 5;
    int y = 7;

    int z = 3 + x * x;
    printnum(z);
// > 28

    printnum(z + y);
// > 35
}

void test2() {
    int x = 5;
    int y = 7;

    printnum(x / y);
// > 0

    printnum(y / x + 1);
// > 2

    printnum(58 % 2);
// > 0

    printnum(58 % 3);
// > 1
}

void test3() {
    int x = 42;
    int v = 21454 - (484878 - x) * 454 + (545 / 56) + 81 % 10 + x;
    printnum(v);
// > -220094038
}

void test4() {
   printnum(4545 * (45 / 5456) % 4554 + 5454 - 871);
// > 4583
}

void test5() {

}

int main() {
    test1();
    test2();
    test3();
    test4();
    test5();

    return 0;
}