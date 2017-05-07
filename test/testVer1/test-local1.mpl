int foo()
{
    int j; 
/* Should hide the formal i */
    int k;
    j = 42; 
    k = j + j;
    print(k);
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
