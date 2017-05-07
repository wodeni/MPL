/*
 * Demo program for COMS 4115
 */

int evolve() {
    int i;
    int sum;
    i = 0;

    /* compute the number of neighbors alive */
    sum = #NW + #N + #NE + #W + #E + #SW + #S + #SE;

    if(#C == 1)
        if(sum == 2 || sum == 3) return 1;
        else return 0;
    else
        if(sum == 3) return 1;
        else return 0;
}

int main() {
    Mat<int>[100][100] board;
    matread("gun.bin", board);

    while(true) {
        print_board(board, 100);
        evolve @ board;
    }
}
