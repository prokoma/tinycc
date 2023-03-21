struct s {
    int a;
    int b;
};

int main() {
    s ar[10];

    printnum(&(ar[5]) - &(ar[1]));
// > 4

    printnum(&(ar[1]) == ar + 1);
// > 1

    printnum(ar + 2 == &(ar[6]) - 4);
// > 1

    printnum(&(ar[1]) > cast<s*>(0));
// > 1

    printnum(&(ar[1]) >= ar);
// > 1

    printnum(&(ar[1]) < ar);
// > 0

    printnum(&(ar[3]) <= ar + 3);
// > 1

    return 0;
}