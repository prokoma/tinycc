void foo(char c) {
    printnum(c == 1000);
// > 0
    printnum(c < 128);
// > 1
}

void test1() {
    int x = 1000;
    foo(x);
}

char capitalize(char c) {
    return c - 'a' + 'A';
}

void test2() {
    print(capitalize('h'));
    print(capitalize('e'));
    print(capitalize('l'));
    print(capitalize('l'));
    print(capitalize('o'));
    print('\n');
// > HELLO
}

int main() {
    test1();
    test2();

    return 0;
}