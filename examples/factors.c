void factorize(int n) {
    while(n > 0 && n % 2 == 0) {
        printnum(2);
        n = n >> 1;
    }
    for(int i = 3; i * i <= n; i = i + 2) {
        while(n % i == 0) {
            printnum(i);
            n = n / i;
        }
    }
    if(n > 1)
        printnum(n);
}


int main() {
    factorize(10);
// > 2
// > 5

//    factorize(9223372036854775807);
    factorize(68767889);
// > 31
// > 2218319

    return 0;
}
