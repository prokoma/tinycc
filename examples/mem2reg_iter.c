// when px is elliminated, x can be elliminated too

int main() {
    int x = 5;
    int * px = &x;
    *px = 10;
    printnum(*px);
    return 0;
}

// > 10
