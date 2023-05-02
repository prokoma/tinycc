int collatz(int n) {
	int i = 0;
	while(n > 1) {
		if(n & 1) {
			n = 3 * n + 1;
		} else {
			n = n / 2;
		}
		i++;
	}
	return i;
}

int main() {
	printnum(collatz(31));
// > 106

	printnum(collatz(42));
// > 8

	printnum(collatz(1000));
// > 111

	return 0;
}
