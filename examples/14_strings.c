void prints(char * s) {
    while(*s) {
        print(*s);
//        s = cast<char*>(cast<int>(s) + 1);
        s = s + 1;
    }
}

int strlen(char * s) {
    int len = 0;
    while(*s) s++, len++;
    return len;
}

int strcmp(char * a, char * b) {
    while(*a && *b) {
        if(*a < *b) return -1;
        else if(*a > *b) return 1;
        a++, b++;
    }
    if(*a) return 1;
    else if(*b) return -1;
    return 0;
}

int main() {
    char * hello = "Hello, world!\n";
    prints(hello);
// > Hello, world!

    printnum(strlen(hello));
// > 14

    prints("== strcmp test ==\n");
// > == strcmp test ==

    printnum(strcmp("correct", "horse"));
// > -1

    printnum(strcmp("horse", "correct"));
// > 1

    printnum(strcmp("horse", "horse"));
// > 0

    return 0;
}