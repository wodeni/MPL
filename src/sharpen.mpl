int entry() {
    int sum;
    sum = #N + #W + #E + #S;
    sum = #C * 5 - sum;
    return sum;
}

int main() {
    Mat<int>[512][512] img;
    pgmread("lena.pgm", img);
    entry @ img;
    pgmwrite("lena-out.pgm", img);
}
