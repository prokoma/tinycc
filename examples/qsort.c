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

void qsort(int * ar, int n) {
    if(n < 2)
        return;
    int pivot = ar[n-1];
    int i = -1;
    for(int j = 0; j < n-1; j++) {
        if(ar[j] <= pivot) {
            i++;
            int t = ar[i];
            ar[i] = ar[j];
            ar[j] = t;
        }
    }
    i++;
    int t = ar[i];
    ar[i] = ar[n-1];
    ar[n-1] = t;

    qsort(ar, i-1);
    qsort(ar + (i+1), n - (i+1));
}

int main() {
    srand(43);

    int ar[10];
    int n = 10;

    for(int i = 0; i < n; i++) {
        ar[i] = rand() % 30;
    }

    print_int_arr(ar, n);
    qsort(ar, n);
    print_int_arr(ar, n);

    return 0;
}

// > 4
// > 13
// > 22
// > 13
// > 28
// > 21
// > 26
// > 29
// > 22
// > 19
// >
// > 4
// > 13
// > 13
// > 19
// > 21
// > 22
// > 22
// > 26
// > 28
// > 29
// >

