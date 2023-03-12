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

int main() {
    char * hello = "Hello, world!\n";
    prints(hello);
// > Hello, world!

    printnum(strlen(hello));
// > 14

    return 0;
}