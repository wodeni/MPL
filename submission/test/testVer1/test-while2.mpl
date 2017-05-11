int foo()
{
    int j;
    int k;
    j = 0;
    k = #C;
    while (k > 0) {
      j = j + 2;
      k = k - 1;
    }
    return j;
}

int main()
{
    Mat<int>[3][3] m;
    m = [1,2,3;4,5,6;7,8,9];
    printm(m);
    foo @ m;
    printm(m);
    return 0;
}
