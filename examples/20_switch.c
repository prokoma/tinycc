void print_until_10(int n) {
	switch(n) {
		case 1: printnum(1);
		case 2: printnum(2);
		case 3: printnum(3);
		case 4: printnum(4);
		case 5: printnum(5);
		case 6: printnum(6);
		case 7: printnum(7);
		case 8: printnum(8);
		case 9: printnum(9);
		case 10: printnum(10); break;
		default: printnum(42);
	}
}

int div_by_2(int n) {
	int res;
	switch(n) {
		case 9:
		case 8:
			res = 4;
			break;
		case 7:
		case 6:
			return 3;
		case 5:
		case 4:
			res = 2;
			break;
		case 3:
		case 2:
			return 1;
		default:
			res = 0;
	}
	return res;
}


int main() {
	printnum(div_by_2(9));
	printnum(div_by_2(6));
	printnum(div_by_2(5));
	printnum(div_by_2(2));
	printnum(div_by_2(0));
	print_until_10(7);
	print_until_10(100);
	return 0;
}

// > 4
// > 3
// > 2
// > 1
// > 0
// > 7
// > 8
// > 9
// > 10
// > 42
