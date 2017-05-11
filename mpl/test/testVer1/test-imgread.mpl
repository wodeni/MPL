int main() {
    Mat<int>[512][512] img;
    pgmread("test/testVer1/lena.pgm", img);
    printm(img);
}
