int fibIter(int n) {
	int a = 0;
	int b = 1;
	for(int i = 0; i < n; i++) {
		int tmp = a;
		a = b;
		b = b + tmp;
	}
	return a;
}

int fibIter2(int n) {
	int a = 0;
	int b = 1;
	int i = 0;
	while(i < n) {
		int tmp = a;
		a = b;
		b = b + tmp;
		i++;
	}
	return a;
}

int fibIter3(int n) {
	int a = 0;
	int b = 1;
	int i = 0;
	do {
		if(i >= n)
			break;
		int tmp = a;
		a = b;
		b = b + tmp;
		i++;
	} while(1);
	return a;
}

int fibRec(int n) {
	if(n <= 0)
		return 0;
	if(n == 1)
		return 1;
	return fibRec(n-1) + fibRec(n-2);
}

int main() {
	printnum(fibIter(42));
// > 267914296
	printnum(fibIter2(42));
// > 267914296
	printnum(fibIter3(42));
// > 267914296
	printnum(fibRec(8));
// > 21

	return 0;
}