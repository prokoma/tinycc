int test1() {
	printnum(1);
// > 1
	return 0;
	printnum(2);
}

int test2() {
	if(1) {
		printnum(3);
// > 3
		return 1;
	} else {
		return 0;
	}
	printnum(4);
}

int main() {
	test1();
	test2();

	return 0;
}
