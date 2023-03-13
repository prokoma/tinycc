struct my_struct {
    int num;
    int ar[2];
};

void print_struct(my_struct* s) {
    printnum(s->num);
    printnum(s->ar[0]);
    printnum(s->ar[1]);
}

my_struct modify_struct(my_struct s) {
    s.num++;
    return s;
}

my_struct global;

int main() {
    my_struct first;
    my_struct second;

    first.num = 42;
    first.ar[0] = 1;
    first.ar[1] = 2;

    print_struct(&first);
// > 42
// > 1
// > 2

    second = first;
    first.ar[0] = 69;

    print_struct(&first);
// > 42
// > 69
// > 2

    print_struct(&second);
// > 42
// > 1
// > 2

    my_struct third = modify_struct(first);

    print_struct(&first);
// > 42
// > 69
// > 2

    print_struct(&third);
// > 43
// > 69
// > 2

    return 0;
}