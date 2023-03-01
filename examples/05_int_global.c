int pi;

int get_pi() {
    return pi;
}

int pi = 314;

int get_42() {
    int pi = 4546545;
    pi = 2;
    return pi * 21;
}

int main() {
    printnum(get_pi());
// > 314

    pi = 45466;
    printnum(get_pi());
// > 45466

    printnum(get_42());
// > 42

    return 0;
}