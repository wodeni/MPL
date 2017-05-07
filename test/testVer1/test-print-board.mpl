int evolve() {
    int i;
    int sum;
    i = 0;

    sum = #NW + #N + #NE + #W + #E + #SW + #S + #SE;

    if(#C == 1)
        if(sum == 2 || sum == 3)
            return 1;
        else
            return 0;
    else
        if(sum == 3)
            return 1;
        else
            return 0;
}

int main() {
    Mat<int>[100][100] board;
    int i;

    i = matread("test/testVer1/gun.bin", board);
    if(i == 0)
        print_board(board, 100);
    else
        prints("File not found");
}


