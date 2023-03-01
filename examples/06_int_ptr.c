int main() {
	int x;

	int *y = &x;
	*y = 5;

	printnum(x);
	return 0;
}

// Expected output:
// > 5