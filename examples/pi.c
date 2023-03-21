void print_int_4(int n) {
    print(48 + (n / 1000) % 10);
    print(48 + (n / 100) % 10);
    print(48 + (n / 10) % 10);
    print(48 + (n / 1) % 10);
}

// https://crypto.stanford.edu/pbc/notes/pi/code.html

// modified from 2800 (200*14) to 70 (5*14) because of limited RAM
// computes 20 digits of PI

int main() {
    int r[71];
    int i, int k;
    int b, int d;
    int c = 0;

    for (i = 0; i < 70; i++) {
        r[i] = 2000;
    }

    for (k = 70; k > 0; k = k - 14) {
        d = 0;

        i = k;
        for (;;) {
            d = d + r[i] * 10000;
            b = 2 * i - 1;

            r[i] = d % b;
            d = d / b;
            i--;
            if (i == 0) break;
            d = d * i;
        }
        print_int_4(c + d / 10000);
        c = d % 10000;
    }

    print('\n');
    return 0;
}

// > 31415926535897932384
