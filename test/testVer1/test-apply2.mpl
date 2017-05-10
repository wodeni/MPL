int entryf() {
    return #N;
}

int main() {
    Mat<int>[3][3] m;
    int p;
    m = [1,2,3;4,5,6;7,8,9];
    entryf @ m;
    printm(m);
    return 0;
}
