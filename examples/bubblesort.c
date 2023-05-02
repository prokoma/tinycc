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

void swap(int * a, int * b) {
    int t = *a;
    *a = *b;
    *b = t;
}

void bubblesort(int * ar, int n) {
    if(n < 2)
        return;
    int sorted;
    do {
        sorted = 1;
        for(int i = 1; i < n; i++) {
            if(ar[i-1] > ar[i]) {
                sorted = 0;
                swap(&ar[i-1], &ar[i]);
            }
        }
    } while(!sorted);
}

int main() {
    srand(43);

    int ar[10];
    int n = 10;

    for(int i = 0; i < n; i++) {
        ar[i] = rand() % 1000;
    }

    print_int_arr(ar, n);
    bubblesort(ar, n);
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

