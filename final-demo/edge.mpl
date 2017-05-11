int entry() {
    int sum;
    sum = #NW + #N + #NE + #W + #S + #E + #SW + #SE;
    sum = #C * 8 - sum;
    if(sum < 0) 
        sum = 0;
    return sum;
}

int main() {
    Mat<int>[512][512] img;
    pgmread("lena.pgm", img);
    entry @ img;
    pgmwrite("lena-out.pgm", img);
}
