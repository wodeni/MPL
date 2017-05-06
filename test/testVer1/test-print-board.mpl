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
    matread("gun.txt", board);
    print_board(board, 100);
}


