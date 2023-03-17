char cbuf = 0;
char getc() {
    if(cbuf == 0)
        cbuf = scan();
    char c = cbuf;
    cbuf = 0;
    return c;
}
char peekc() {
    if(cbuf == 0)
        cbuf = scan();
    return cbuf;
}
void ungetc(char c) {
    cbuf = c;
}

int isspace(char c) {
    return c == ' ' || c == '\r' || c == '\n' || c == '\t';
}

void skipws() {
    while(isspace(peekc()))
        getc();
}

void print_int(int n) {
    if(n < 0) {
        print('-');
        n = -n;
    }
    if(n >= 10)
        print_int(n / 10);
    print(n % 10 + '0');
}

void print_str(char * str) {
    while(*str) {
        print(*str);
        str++;
    }
}

int scan_int() {
    int res = 0, int sgn = 1;
    skipws();
    char c = getc();
    if(c == '-') {
        sgn = -1;
        c = getc();
    }
    while(c >= '0' && c <= '9') {
        res = res * 10 + (c - '0');
        c = getc();
    }
    ungetc(c);
    return sgn * res;
}

// ----------------------------

// https://onlinejudge.org/external/106/10646.pdf

// <! p10646.in
// >! p10646.ref

char cards[104];
int num_cards = 52;

int get_value(int i) {
    char c = cards[i * 2];
    if(c >= '2' && c <= '9')
        return c - '0';
    return 10;
}

void print_card(int i) {
    print(cards[i * 2]);
    print(cards[i * 2 + 1]);
}

int play_game() {
    int top = num_cards;
    // Take the top 25 cards of the pile in the hand.
    top = top - 25;
    int hand_start = top;

    // Set Y = 0.
    int Y = 0;
    // Then execute three times the following steps together:
    for(int i = 0; i < 3; i++) {
        // Take the top card of the cards of the pile and determine its value.
        int X = get_value(top - 1);
        // Let the card value be X. Add X to Y.
        Y = Y + X;
        // Put this card and the top (10 − X) cards of the pile away.
        top = top - 1 - (10 - X);
    }
    // At last put the 25 remaining cards in your hand on top of the pile. The task is to determine the Y -th card from the pile, counted from the bottom.
    if(Y - 1 < top)
        return Y - 1;
    return (Y - 1) - top + hand_start;
}

int main() {
    int tc = scan_int();
    for(int i = 0; i < tc; i++) {
        for(int j = 0; j < num_cards * 2;) {
            skipws();
            cards[j++] = getc();
            cards[j++] = getc();
        }

        print_str("Case ");
        print_int(i + 1);
        print_str(": ");
        print_card(play_game());
        print('\n');
    }
    return 0;
}