void test1() {
    int numbers[5];
    numbers[0] = 1;
    numbers[1] = 2;

    printnum(numbers[0]);
    printnum(numbers[1]);
// > 1
// > 2

    for(int i = 0; i < 2; i++)
        printnum(numbers[i]);
// > 1
// > 2
}

void test2() {
    int numbers[5];
    numbers[2] = 3;
    int x = 1;
    numbers[x] = 2;
    numbers[0] = 1;
    numbers[numbers[2]] = 4;
    numbers[4] = 5;

    for(int i = 0; i < 5; i++)
        printnum(numbers[i]);
// > 1
// > 2
// > 3
// > 4
// > 5
}

int main() {
    test1();
    test2();

    return 0;
}