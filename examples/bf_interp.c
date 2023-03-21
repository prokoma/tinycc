int code[512];
int mem[128];
int head = 0;

void print_str(char * str) {
    while(*str) {
        print(*str);
        str++;
    }
}

int read_program() {
    int stack[64];
    int j = 0;
    for(int i = 0; ; i++) {
        char c = scan();
        if(c == -1 || c == '!') {
            if(j != 0) {
                print_str("Unmatched [");
            }
            return i;
        }
        code[i] = c;
        if(c == '[') {
            stack[j++] = i;
        } else if(c == ']') {
            if(j == 0) {
                print_str("Unmatched ]");
                return 0;
            }
            int t = stack[--j];
            code[t] = code[t] | (i << 32); // store jump addr in high 32 bits
            code[i] = code[i] | (t << 32);
        }
    }
}

char read_mem() {
    int idx = head >> 3;
    int shift = ((head & 7) << 3); // 0, 8, 16, 24, 32, 48, 56
    return (mem[idx] >> shift) & 255;
}

void write_mem(char v) {
    int idx = head >> 3;
    int shift = ((head & 7) << 3); // 0, 8, 16, 24, 32, 48, 56
    mem[idx] = (mem[idx] & ~(255 << shift)) | (cast<int>(v) << shift);
}

int main() {
    int len = read_program();
    int ip = 0;
    while(ip < len) {
        char m = read_mem();
        switch(code[ip] & 255) {
            case 43: // +
                write_mem(m + 1);
                break;
            case 45: // -
                write_mem(m - 1);
                break;
            case 62: // >
                head++;
                break;
            case 60: // <
                head--;
                break;
            case 91: // [
                if(!m) ip = code[ip] >> 32;
                break;
            case 93: // ]
                if(m) ip = code[ip] >> 32;
                break;
            case 46: // .
                print(m);
                break;
            case 44: // ,
                char c = scan();
                if(c != -1) write_mem(c);
                break;
        }
        ip++;
    }

    return 0;
}
//x
//// < ,>,>,>,>,>,>,>,>,>,>,>,>,.<.<.<.<.<.<.<.<.<.<.<.<.!Hello, world!
//// > !dlrow ,olleH

// https://cs.wikipedia.org/wiki/Brainfuck#K%C3%B3d_bez_koment%C3%A1%C5%99e

// < ++++++++++[>+++++++>++++++++++>+++>+<<<<
// < -]>++.>+.+++++++..+++.>++.<<++++++++++++
// < +++.>.+++.------.--------.>+.>.

// > Hello World!