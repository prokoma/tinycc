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

int array_sum(int * ar, int n) {
    int sum = 0;
    for(int i = 0; i < n; i++)
        sum = sum + ar[i];
    return sum;
}

int array_sum2(int * ar, int n) {
    int sum = 0;
    while(n--) {
        sum = sum + ar[n];
    }
    return sum;
}

int global_arr[4];

void test3() {
    int i = 0;
    global_arr[i++] = 12;
    global_arr[i++] = 42;
    global_arr[i++] = 5;
    global_arr[i++] = 1;

    printnum(global_arr[0]);
    printnum(global_arr[1]);
    printnum(global_arr[2]);
    printnum(global_arr[3]);
// > 12
// > 42
// > 5
// > 1

    printnum(array_sum(global_arr, 4));
// > 60

    printnum(array_sum2(global_arr, 4));
// > 60
}

int main() {
    test1();
    test2();
    test3();

    return 0;
}