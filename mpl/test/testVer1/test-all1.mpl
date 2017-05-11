int entryf() {
    return 1;
}

int main() {
    Mat<int>[3][3] m;
    int p;
    m = [1,2,3;4,5,6;7,8,9];
    entryf @ m;
    p = m[1][1];
    print(p);
    return 0;
}
