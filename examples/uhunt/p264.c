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

int min(int a, int b) {
    if(a < b) return a;
    else return b;
}

int max(int a, int b) {
    if(a > b) return a;
    else return b;
}

int msb(int x) {
    int r = 0;
    while(x = x >> 1) {
        r++;
    }
    return r;
}

double DBL_EPSILON = 2.2204460492503131e-016;

double fabs(double x) {
    if(x < 0) return -x;
    else return x;
}

double sqrt(double x) {
    double r = 1;
    for(int i = 0; i < 10; i++)
        r = r - (r*r - x) / (2*r);
    return r;
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

void print_fixed(double n, int decimals) {
    int int_part = n;
    double frac_part = fabs(n - int_part);
    print_int(n);
    if(decimals <= 0 || frac_part < DBL_EPSILON) return;
    print('.');
    while(decimals-- > 0 && frac_part >= DBL_EPSILON) {
        frac_part = frac_part * 10;
        int digit = frac_part;
        print('0' + digit);
        frac_part = frac_part - digit;
    }
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

// ----------------------------

// https://onlinejudge.org/external/2/264.pdf

// < 1
// < 3
// < 14
// < 7

// > TERM 1 IS 1/1
// > TERM 3 IS 2/1
// > TERM 14 IS 2/4
// > TERM 7 IS 1/4

// <! p264.in
// >! p264.ref

int main() {
    while(peekc() != -1) {
        int s = scan_int()-1;
        int n = (sqrt(1+8*s)-1)/2;
        int a = s - n*(n+1)/2;

//        printnum(n);
//        printnum(a);

        print_str("TERM ");
        print_int(s+1);
        print_str(" IS ");
        if(n % 2 == 0) {
            print_int(n+1 - a);
            print('/');
            print_int(a+1);
        } else {
            print_int(a+1);
            print('/');
            print_int(n+1 - a);
        }
        print('\n');

        skipws();
    }

    return 0;
}