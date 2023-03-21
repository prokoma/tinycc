int main() {
	int is_prime[50];
	for(int i = 0; i < 50; i++)
		is_prime[i] = 1;
	for(int i = 2; i < 50; i++) {
		if(is_prime[i]) {
			printnum(i);
			for(int j = i + i; j < 50; j = j + i)
				is_prime[j] = 0;
		}
	}

	for(int i = 2; i < 50; i++) {
		if(is_prime[i])
			printnum(i);
	}

	return 0;
}

// > 2
// > 3
// > 5
// > 7
// > 11
// > 13
// > 17
// > 19
// > 23
// > 29
// > 31
// > 37
// > 41
// > 43
// > 47
// > 2
// > 3
// > 5
// > 7
// > 11
// > 13
// > 17
// > 19
// > 23
// > 29
// > 31
// > 37
// > 41
// > 43
// > 47
