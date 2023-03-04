void test1() {
    double x = 1.1;
    printnum(1);
// > 1
}

void test2() {
    double x = 10.5;
    double y = 20;
    printnum((x + y) * 100);
// > 3050
}

void test3() {
    double x = 3.14;
    int y = x;
    printnum(y);
// > 3
}

int main() {
    test1();
    test2();
    test3();

    return 0;
}