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

int main() {
    printnum(sum(square(pi100()), negate(cube(4*pi100()))));
// > -1981286620

    return 0;
}