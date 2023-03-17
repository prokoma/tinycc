int main() {
    int x = 1 == 2;

    // this tests freezing functionality in MaximalMunch

    // 2. and then this wants result of just the cmp, which is already covered, so it fails
    printnum(x);
    // 1. this covers both condbr and cmp
    if(x) {
        printnum(1);
    } else {
        printnum(0);
    }

    return 0;
}

// > 0
// > 0