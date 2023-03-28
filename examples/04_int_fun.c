int pi100() {
    return 314;
}

int sum(int a, int b) {
    return a + b;
}

int square(int x);

int cube(int x) {
    return x * square(x);
}

int square(int x) {
    return x * x;
}

int negate(int x) {
    return -x;
}

int sub(int a, int b) {
    return a - b;
}

void test1() {
    printnum(pi100());
// > 314

    printnum(square(4));
// > 16
}

void test2() {
    printnum(sub(456, 42));
// > 414

    printnum(sub(100, square(4)));
// > 84
}

void test3() {
    printnum(sum(square(pi100()), negate(cube(4*pi100()))));
// > -1981286620
}

void test4() {
    printnum(negate(5));
// > -5
}

int main() {
    test1();
    test2();
    test3();
    test4();

    return 0;
}