int print1(int x) {
    printnum(1);
    return x;
}

int print0(int x) {
    printnum(0);
    return x;
}

void test1() {
    if(1) {
        printnum(1);
    } else {
        printnum(0);
    }
// > 1

    int x = 5;
    if(x == 5) {
        printnum(1);
    } else {
        printnum(0);
    }
// > 1
}

void test2() {
    if((print0(0) && print1(1)) == 0) {
// > 0
        printnum(1);
    } else {
        printnum(0);
    }
// > 1
}

void test3() {
    int y = 0;
    for(int x = 0; x < 10; x++) {
        y = y + 2;
    }
    printnum(y);
// > 20
}

void test4() {
    int y = 20;
    for(;;) {
        y = y - 1;
        if(y == 5)
            break;
    }
    printnum(y);
// > 5
}

void test5() {
    while(0) {
        printnum(1);
    }

    do {
        printnum(2);
    } while(0);
// > 2
}

void test6() {
    int x = 0;
    while(x < 10) {
        x = x + 1;
    }
    printnum(x);
// > 10
}

void test7() {
    for(int i = 0; ; i++) {
        if(i % 2 == 0)
            continue;
        if(i >= 10)
            break;
        printnum(i);
    }
// > 1
// > 3
// > 5
// > 7
// > 9
}

int main() {
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();

    return 0;
}