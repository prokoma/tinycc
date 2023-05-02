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

int RAND_MAX = 2147483647;
int rand_state = 0;
void srand(int seed) {
    rand_state = seed;
}
int rand() {
    // standard LCG constants used in many rand() implementations
    rand_state = (rand_state * 1103515245 + 12345) & 2147483647;
    return rand_state;
}

int main() {
    print_fixed(-10.5, 5);
    print('\n');
// > -10.5

    print_fixed(-10.534, 2);
    print('\n');
// > -10.53

    print_int(msb(1));
    print('\n');
// > 0

    print_int(msb(2));
    print('\n');
// > 1

    print_int(msb(3));
    print('\n');
// > 1

    print_int(msb(4));
    print('\n');
// > 2

    print_fixed(sqrt(2), 5);
    print('\n');
// > 1.41421

    print_fixed(sqrt(10), 5);
    print('\n');
// > 3.16227

    srand(10);
    print_int(rand());
    print('\n');
    print_int(rand());
    print('\n');
    print_int(rand());
    print('\n');
// > 297746555
// > 1849040536
// > 736986865

    return 0;
}
