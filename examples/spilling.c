int main() {
	int x = 45 + (454 * 56) + 568;
	int y = (45 + x + x) * x;
	int z = 20 + (x - y) + y;
	int t = x + z - (x - z + (y - x)) + 455;
	int u = x + y + (((x + 565 + y + t) + y) + 56 + 78) + t;
	printnum(x);
	printnum(y);
	printnum(z);
	printnum(t);
	printnum(u);

	return 0;
}

// > 26037
// > 1357022403
// > 26057
// > -1356943797
// > 1357232388
