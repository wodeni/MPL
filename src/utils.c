/*
 * File: utils.c
 * Date: 2017-04-23
 *
 * PLT Spring 2017
 * MPL Project
 * Wode "Nimo" Ni    <wn2155@columbia.edu>
 * David Rincon-Cruz <dr2884@columbia.edu>
 * Chi Zhang         <cz2440@columbia.edu>
 * Jiangfeng Wang    <jw3107@columbia.edu>
 */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

// #define LIVE 254 // a black square
#define LIVE "\u2588\u2588" // a black square
#define DEAD "  "     // a space
#define clear() printf("\033[H\033[J")
// #define clear() puts("\033[J\033[1;1H")
// #define clear() system ( "clear" );
#define get_symbol(i) (i == 0 ? DEAD : LIVE)
#define mat_entry(mat, n, i, j) (*((mat + i * n) + j))

// #define DEBUG

/* Given a board of Conway's Game of life, pretty print it. 
 * @mat: the board
 * @m  : the width of the matrix
 * @n  : the height of the matrix
 * @sleep: The time interval between updates, in milliseconds
 */
void print_board(int* mat, int m, int n, int sleep) {
    fflush(stdout);
    clear();
    
    for(int i = 0; i < m; ++i) {
        for(int j = 0; j < n; ++j) {
            int entry = mat_entry(mat, n, i, j);
            if(j == n - 1) 
                printf("%s\n", get_symbol(entry));
            else 
                printf("%s", get_symbol(entry));
            fflush(stdout);
        }
    }
    usleep(sleep * 1000); // Sleep for 0.5s
}


/* Pretty-print out an integer matrix to stdout
 * @mat: the pointer to the starting address of the matrix
 * @m  : the width of the matrix
 * @n  : the height of the matrix
 */
void printm_int(int* mat, int m, int n) {
    printf("[\n");
    fflush(stdout);
    for(int i = 0; i < m; ++i) {
        for(int j = 0; j < n; ++j) {
            if(j == n - 1) {
                printf("%d;\n", *((mat+i*n) + j));
                fflush(stdout);
            }
            else {
                printf("%d, ", *((mat+i*n) + j));
                fflush(stdout);
            }
        }
    }
    printf("]\n");
}


/* Pretty-print out a float matrix to stdout
 * @mat: the pointer to the starting address of the matrix
 * @m  : the width of the matrix
 * @n  : the height of the matrix
 */
void printm_float(double* mat, int m, int n) {
    printf("[");
    fflush(stdout);
    for(int i = 0; i < m; ++i) {
        for(int j = 0; j < n; ++j) {
            if(j == n - 1) {
                printf("%f; ", *((mat+i*n) + j));
                fflush(stdout);
            }
            else {
                printf("%f, ", *((mat+i*n) + j));
                fflush(stdout);
            }
        }
    }
    printf("]\n");
}

/* Read in an integer matrix from a file designated by a path. 
 * The input file should be a linear listing of entries in the 
 * matrix in row-major order.
 * @path: the path to the input file
 * @mat : the pointer to the starting address of the output matrix
 * @row : the width of the matrix
 * @col : the height of the matrix
 */
int matread_int (char* path, int* mat, int row, int col){
	FILE* fd = fopen(path,"r");
	if(fd==NULL)
		return -1;
	int count = 0;
 	while(fread(mat, 1, 4, fd)==4){
 		mat++;
		count++;
		if(count==row*col){
			fclose(fd);
			return 0;
		}
	} 
	fclose(fd);
	return -2;	
}

/* Read in a float matrix from a file designated by a path. 
 * The input file should be a linear listing of entries in the 
 * matrix in row-major order.
 * @path   : the path to the input file
 * @mat    : the pointer to the starting address of the output matrix
 * @row    : the width of the matrix
 * @col    : the height of the matrix
 * @return : 0 on success, -1 on file-not-fount error
 */
int matread_float (char* path, double* mat, int row, int col){
	FILE* fd = fopen(path,"r");
	if(fd==NULL)
		return -1;
	int count = 0;
 	while(fread(mat, 1,sizeof(double), fd)==sizeof(double)){
		count++;
		mat++;
		if(count==row*col){
			fclose(fd);
			return 0;
		}
	} 
 	fclose(fd);
	return -2;	// FIXME: file size not enough
}

/* Write to an int matrix from a file designated by a path. 
 * The output file should be a linear listing of entries in the 
 * matrix in row-major order.
 * @path   : the path to the output file
 * @mat    : the pointer to the starting address of the input matrix
 * @row    : the width of the matrix
 * @col    : the height of the matrix
 * @return : 0 on success, -1 on any I/O error
 */
int matwrite_int (char *path, int *mat, int row, int col) {
	FILE* fd = fopen(path, "w");
	if(fd == NULL) 
		return -1;
    int size = row * col * sizeof(int);
    if(fwrite(mat, size, 1, fd) != 1) {
        printf("should not happen\n");
        return -1;
    }
    fclose(fd);
    return 0;
}

/* Write to an float matrix from a file designated by a path. 
 * The output file should be a linear listing of entries in the 
 * matrix in row-major order.
 * @path   : the path to the output file
 * @mat    : the pointer to the starting address of the input matrix
 * @row    : the width of the matrix
 * @col    : the height of the matrix
 * @return : 0 on success, -1 on any I/O error
 */
int matwrite_float (char *path, double *mat, int row, int col) {
	FILE* fd = fopen(path, "w");
	if(fd == NULL) 
		return -1;
    int size = row * col * sizeof(double);
    if(fwrite(mat, size, 1, fd) != 1) {
        printf("should not happen\n");
        return -1;
    }
    fclose(fd);
    return 0;
}


#ifdef DEBUG
int main() {
    int mat[3][3] = {
        {0, 0, 1},
        {0, 1, 0},
        {1, 0, 0},
    };
    print_board((int *)mat, 3, 3);
    return 0;
}
#endif
