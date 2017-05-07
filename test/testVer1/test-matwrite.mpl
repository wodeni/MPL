int main() {
    Mat<int>[3][3] m1;
    Mat<int>[3][3] m2;
    m1 = [1,2,3;4,5,6;7,8,9];
    matwrite("/test/testVer1/matoutput.txt",m1);
    matread("/test/testVer1/matoutput.txt",m2);
    printm(m2);

    return 0;
}
