int main() {
    Mat<int>[512][512] img;
    pgmread("lena.pgm", img);
    printm(img);
}
