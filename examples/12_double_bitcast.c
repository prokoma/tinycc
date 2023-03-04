void test1() {
    double pi = 3.14;
    int pi2 = *cast<int*>(&pi);
    double pi3 = *cast<double*>(&pi2);
    printnum(pi == pi3);
// > 1
}

int main() {
    test1();
    return 0;
}