void test1() {
	int x;
	int *y = &x;
	*y = 5;

	printnum(x);
// > 5
}

void swapIntPtr(int **ppx, int **ppy) {
    int *tmp = *ppx;
    *ppx = *ppy;
    *ppy = tmp;
}

void test2() {
    int x = 7;
    int y = 8;
    int *px = &x;
    int *py = &y;

    printnum(*px);
// > 7
    printnum(*py);
// > 8

    swapIntPtr(&px, &py);
    printnum(*px);
// > 8
    printnum(*py);
// > 7
}

int main() {
    test1();
    test2();

	return 0;
}