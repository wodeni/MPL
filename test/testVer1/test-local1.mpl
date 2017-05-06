int foo()
{
    int i; 
/* Should hide the formal i */

    i = 42; 
    print(i + i);
    return 42;
}

int main()
{
    Mat<int>[3][3] m;
    int i;
    i = 8;
    print(i);
    m = [1,1,1;1,1,1;1,1,1];
    printm(m);
    foo @ m;
    print(i);
    printm(m);
    return 0;
}
