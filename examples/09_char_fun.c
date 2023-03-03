void foo(char c) {
    printnum(c == 1000);
// > 0
    printnum(c < 128);
// > 1
}

int main() {
    int x = 1000;
    foo(x);

    return 0;
}