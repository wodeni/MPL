int entryf() {
    return 1;
}

int main() {
    Mat<int>[2][2] m;
    m = [1,2;3,4];
    /*matread("matexample.txt",m);*/
    printm(m);
    
    entryf @ m;
    return 0;
}
