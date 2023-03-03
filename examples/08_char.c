void test1() {
    int i = 10000;
    char c = i;
    if(c < 128)
        printnum(1);
    else
        printnum(0);
// > 1
}

void test2() {
    int x = 'a';
    while(x <= 'z') {
        print(x);
        x = x + 1;
    }
    print('\n');
// > abcdefghijklmnopqrstuvwxyz
}

void test3() {
    int x = -128;
    char c = cast<char>(x);
    int y = c;

    printnum(x);
// > -128
    printnum(c);
// > -128
    printnum(y);
// > -128

    printnum(x == c);
// > 1
    printnum(x == y);
// > 1
    c = c - 1;
    printnum(x == c);
// > 0
}

void test4() {
    int x = 127;
    char c = x;

    printnum(x);
// > 127
    printnum(c);
// > 127

    printnum(x == c);
// > 1
    x = x + 1;
    c = c + 1;
    printnum(x == c);
// > 0
}

char global_char = 'a';
char global_char_2 = 1000;

void test5() {
    printnum(global_char == 'a');
// > 1
    printnum(global_char_2 != 1000);
// > 1
    global_char = 'b';
    printnum(global_char == 'b');
// > 1
}

int main() {
    test1();
    test2();
    test3();
    test4();
    test5();

    return 0;
}