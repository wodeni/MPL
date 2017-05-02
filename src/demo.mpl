int evolve {
    int i;
    int sum;
    i = 0;
    sum = #NW + #N + #NE + #W + #E + #SW + #S + #SE;
    print(#C);
    /* Any live cell with fewer than two live neighbours dies, as if caused by underpopulation. */
    /* Any live cell with more than three live neighbours dies, as if by overpopulation. */
    if(sum < 2 || sum > 3)  
        return 0;

    /* Any live cell with two or three live neighbours lives on to the next generation. */
    /* Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction. */
    if(sum == 2 || sum == 3 || (#C == 0 && sum == 3)) 
        return 1;
}

int main {
    int rounds;
    Mat<int>[3][3] board;
    board = [0, 1, 1; 0, 0, 0; 0, 0, 0];
    rounds = 0;
    printm(board);
    evolve @ board;
    while(rounds < 10) {
        rounds = rounds + 1;
    }
}

/*
        printm(board);
    matread("initial_board.ppm", board);
        evolve @ board;
Mat<int>[8, 1] neighbors;
neighbors = {}
while(i < 8) {
    n = neighbors[i];
    if(n == 1)
        sum++;
}
    while(true) 
*/
