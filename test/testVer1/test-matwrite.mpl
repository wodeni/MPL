/*int entryf {
    return 1;
}
*/
int main {
    Mat<int>[3][3] m1;
    Mat<int>[3][3] m2;
    m1 = [1,2,3;4,5,6;7,8,9];
    matwrite("matoutput.txt",m1);
    matread("matoutput.txt",m2);
    printm(m2);
/*    
    entryf @ m;
    printm(m);
*/
    return 0;
}
