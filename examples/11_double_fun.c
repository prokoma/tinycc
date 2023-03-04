void foo(double x) {
    printnum(x * 2);
}

void test1() {
    foo(1.5);
// > 3
    foo(2);
// > 4
}

double avg(double x, double y) {
    return (x + y) / 2;
}

void test2() {
    double x = 5;
    printnum(avg(x, 2) * 10);
// > 35
}

double global_pi = 3.14;

void test3() {
    double pi = global_pi;
    global_pi = global_pi * 2;
    printnum(pi * 100);
// > 314
    printnum(global_pi * 100);
// > 628
}

int main() {
    test1();
    test2();
    test3();

    return 0;
}