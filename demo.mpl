int evolve {
    int i, sum;
    i = 0;
    /*
    Mat<int>[8, 1] neighbors;
    neighbors = {}
    while(i < 8) {
        n = neighbors[i];
        if(n == 1)
            sum++;
    }
    */
    sum = #NW + #N + #NE + #W + #E + #SW + #S + #SE;
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
    Mat<int>[100, 100] board;
    board = matread("initial_board.ppm");
    while(true)
        evolve @ board;
}
