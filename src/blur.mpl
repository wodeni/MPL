int entry() {
    int sum;
    sum = #NW + 2*#N + #NE + 2*#W + 2*#E + #SW + 2*#S + #SE;
    sum = #C * 4 + sum;
    return sum / 16;
}

int main() {
    Mat<int>[512][512] img;
    pgmread("lena.pgm", img);
    entry @ img;
    pgmwrite("lena-out.pgm", img);
}
