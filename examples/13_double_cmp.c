void test1() {
    double x = 5.0 / 2;
    printnum(x >= 2.4);
// > 1
    printnum(x <= 2.6);
// > 1
}

void test2() {
    printnum(1.6 == 1.6);
// > 1
    printnum(1.0 != 1.0);
// > 0
}

void test3() {
    if(0.1) {
        printnum(1);
    } else {
        printnum(0);
    }
// > 1

    if(1.1) {
        printnum(1);
    } else {
        printnum(0);
    }
// > 1

    if(0.0) {
        printnum(1);
    } else {
        printnum(0);
    }
// > 0

    printnum(!0.5);
// > 0

    printnum(!0.0);
// > 1
}

int main() {
    test1();
    test2();
    test3();
    return 0;
}