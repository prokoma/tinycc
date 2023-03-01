int main() {
	for(int k = 1; k <= 20; k++) {
		if((k % 3 == 0) && (k % 5 == 0)) {
			print('F'); print('i'); print('z'); print('z');
			print('B'); print('u'); print('z'); print('z');
			print('\n');
		} else if(k % 3 == 0) {
			print('F'); print('i'); print('z'); print('z');
			print('\n');
		} else if(k % 5 == 0) {
			print('B'); print('u'); print('z'); print('z');
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
