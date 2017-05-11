int entry() {
   return #S; 
}

int main() {
    Mat<int>[3][3] m;
    Mat<int>[100][100] n;

    int i;
    i = 0;
    m = [1, 2, 3; 4, 5, 6; 7, 8, 9];
    while(true) {
        /* print(i); */
        entry @ m;
        printm(m);
        i = 1 + i;
    }
}
