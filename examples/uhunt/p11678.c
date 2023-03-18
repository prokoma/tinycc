char cbuf = 0;
char getc() {
    if(cbuf == 0)
        cbuf = scan();
    char c = cbuf;
    cbuf = 0;
    return c;
}
char peekc() {
    if(cbuf == 0)
        cbuf = scan();
    return cbuf;
}
void ungetc(char c) {
    cbuf = c;
}

int isspace(char c) {
    return c == ' ' || c == '\r' || c == '\n' || c == '\t';
}

void skipws() {
    while(isspace(peekc()))
        getc();
}

void print_int(int n) {
    if(n < 0) {
        print('-');
        n = -n;
    }
    if(n >= 10)
        print_int(n / 10);
    print(n % 10 + '0');
}

void print_str(char * str) {
    while(*str) {
        print(*str);
        str++;
    }
}

int scan_int() {
    int res = 0, int sgn = 1;
    skipws();
    char c = getc();
    if(c == '-') {
        sgn = -1;
        c = getc();
    }
    while(c >= '0' && c <= '9') {
        res = res * 10 + (c - '0');
        c = getc();
    }
    ungetc(c);
    return sgn * res;
}

int min(int a, int b) {
    if(a < b) return a;
    else return b;
}

int max(int a, int b) {
    if(a > b) return a;
    else return b;
}

// ----------------------------

// https://onlinejudge.org/external/116/11678.pdf

// <! p11678.in
// >! p11678.ref

int array_contains(int el, int * ar, int n) {
    // lower_bound
    int start = 0, int end = n;
    while(end > start) {
        int mid = (start + end) / 2;
        if(ar[mid] < el) {
            start = mid + 1;
        } else {
            end = mid;
        }
    }

    return ar[start] == el;
}

int main() {
    int A, int B;
    int xs[20];
    int ys[20];
    do {
        A = scan_int();
        B = scan_int();

        if(A == 0 && B == 0)
            break;
        for(int i = 0; i < A; i++) xs[i] = scan_int();
        for(int i = 0; i < B; i++) ys[i] = scan_int();

        int an = 0;
        for(int i = 0; i < A; i++) {
            if((i == 0 || xs[i] != xs[i-1]) && !array_contains(xs[i], ys, B))
                an++;
        }
        int bn = 0;
        for(int i = 0; i < B; i++) {
            if((i == 0 || ys[i] != ys[i-1]) && !array_contains(ys[i], xs, A))
                bn++;
        }
        print_int(min(an, bn));
        print('\n');
    } while(1);

    return 0;
}