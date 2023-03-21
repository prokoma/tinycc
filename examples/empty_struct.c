struct empty {};

// replicate gcc behaviour

int main() {
    empty ar[2];

    printnum(&(ar[0]) == &(ar[1]));
// > 1

    printnum(cast<int>(&(ar[1])) - cast<int>(&(ar[0])));
// > 0

    return 0;
}