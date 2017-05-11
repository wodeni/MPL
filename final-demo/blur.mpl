int entry() {
    int sum;
    sum = #NW + 2*#N + #NE + 2*#W + 2*#E + #SW + 2*#S + #SE;
    sum = #C * 4 + sum;
    return sum / 16;
}

int main() {
    Mat<int>[512][512] img;
    int i;
    i = 0;
    pgmread("lena.pgm", img);
    while(i < 20) {
        entry @ img;
        i = i + 1;
    }
    pgmwrite("lena-out.pgm", img);
}
