int brk = 0;
int heap[512];

void* malloc(int n) {
    void * res = &heap[brk];
    brk = brk + n;
    return res;
}

struct list {
    int car;
    list* cdr;
};

list* cons(int car, list* cdr) {
    list* res = cast<list*>(malloc(2));
    res->car = car;
    res->cdr = cdr;
    return res;
}

void print_list(list* l) {
    while(l) {
        printnum(l->car);
        l = l->cdr;
    }
}

int main() {
    list* nil = cast<list*>(0);

    list* l = cons(1, cons(2, cons(3, nil)));
    list* l2 = cons(0, l);

    printnum(l->car);
    printnum(l->cdr->car);
    printnum(l->cdr->cdr->car);
    printnum(cast<int>(l->cdr->cdr->cdr));
// > 1
// > 2
// > 3
// > 0

    printnum(-1);
// > -1

    print_list(l);
// > 1
// > 2
// > 3

    print_list(l2);
// > 0
// > 1
// > 2
// > 3

    return 0;
}