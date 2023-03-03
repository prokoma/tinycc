void Fizz() {
    print('F'); print('i'); print('z'); print('z');
}

void Buzz() {
    print('B'); print('u'); print('z'); print('z');
}

int main() {
	for(int k = 1; k <= 20; k++) {
		if((k % 3 == 0) && (k % 5 == 0)) {
			Fizz();
			Buzz();
			print('\n');
		} else if(k % 3 == 0) {
			Fizz();
			print('\n');
		} else if(k % 5 == 0) {
			Buzz();
			print('\n');
		} else {
			printnum(k);
		}
	}
	return 0;
}

// > 1
// > 2
// > Fizz
// > 4
// > Buzz
// > Fizz
// > 7
// > 8
// > Fizz
// > Buzz
// > 11
// > Fizz
// > 13
// > 14
// > FizzBuzz
// > 16
// > 17
// > Fizz
// > 19
// > Buzz
