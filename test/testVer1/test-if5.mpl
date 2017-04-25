int foo
{
    int j;
    if (#C == 5) 
    	j = 42;
    else
        j = 17;
    return j;
}

int main
{
    Mat<int>[3][3] m;
    m = [1,2,3;4,5,6;7,8,9];
    printm(m);
    foo @ m;
    printm(m);
    return 0;
}
