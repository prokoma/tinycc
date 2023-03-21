// https://www.geeksforgeeks.org/returning-a-function-pointer-from-a-function-in-c-cpp/

typedef int (*ptr)(int*);
typedef ptr (*pm)();

int fun1(int* y)
{
    printnum(2);
    return *y + 10;
}

// Function that return type ptr
ptr get_fun1_ptr() {
    printnum(1);

    return &fun1;
}

// Driver Code
int main()
{
    int a = 10;

    pm u = &get_fun1_ptr;

    printnum((*(*u)())(&a));

    return 0;
}

// > 1
// > 2
// > 20
