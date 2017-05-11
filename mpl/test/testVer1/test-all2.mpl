int entryf() {
    return #C;
}

int main() {
    Mat<int>[2][2] m;
    int p;
    int k;
    k = 1;
    m = [1,2;3,4];
    entryf @ m;
    p = m[0][1];
    print(p);
    return 0;
}
