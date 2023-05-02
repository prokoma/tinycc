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

void print_int_arr(int * ar, int n) {
    for(int i = 0; i < n; i++) {
        printnum(ar[i]);
    }
    print('\n');
}

void mergesort_rec(int * src, int * dest, int n) {
    if(n < 2)
        return;
    int mid = n / 2;

    mergesort_rec(dest, src, mid);
    mergesort_rec(dest + mid, src + mid, n - mid);

    int l = 0;
    int r = mid;
    while(l < mid && r < n) {
        if(src[l] < src[r])
            *(dest++) = src[l++];
        else
            *(dest++) = src[r++];
    }
    while(l < mid) *(dest++) = src[l++];
    while(r < n) *(dest++) = src[r++];
}

void mergesort(int * ar, int * tmp, int n) {
    for(int i = 0; i < n; i++) {
        tmp[i] = ar[i];
    }
    mergesort_rec(tmp, ar, n);
}


int main() {
    srand(43);

    int ar[10];
    int n = 10;

    for(int i = 0; i < n; i++) {
        ar[i] = rand() % 1000;
    }

    print_int_arr(ar, n);
    int tmp[10];
    mergesort(ar, tmp, n);
    print_int_arr(ar, n);

    return 0;
}

// > 624
// > 233
// > 62
// > 663
// > 988
// > 901
// > 826
// > 259
// > 552
// > 289
// >
// > 62
// > 233
// > 259
// > 289
// > 552
// > 624
// > 663
// > 826
// > 901
// > 988
// >

