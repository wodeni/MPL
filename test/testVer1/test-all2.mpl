int entryf(int a) {
    return a+1;
}

int main() {
    Mat<int>[2][2] m;
    int p;
    int k;
    k = 1;
    m = [1,2;3,4];
    entryf @ m;
    p = m[0][0];
    print(p);
    return 0;
}