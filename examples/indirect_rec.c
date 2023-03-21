int bar(int n);

int foo(int n) {
	if(n <= 0)
		return 0;
	return bar(n) + 1;
}

int bar(int n) {
	return foo(n-1) * 2;
}

int main() {
	printnum(foo(3));
	printnum(foo(4));
	printnum(foo(5));
	return 0;
}

// > 7
// > 15
// > 31
