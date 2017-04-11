#include <stdio.h>
// #define BUILD_TEST

void printm_int(int* mat, int m, int n) {
    printf("[");
    fflush(stdout);
    for(int i = 0; i < m; ++i) {
        for(int j = 0; j < n; ++j) {
            if(j == n - 1) {
                printf("%d; ", *((mat+i*n) + j));
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

#ifdef BUILD_TEST
int main()
{
    int m1[2][2]; 
    double m2[2][2]; 
    printm_int((int *)m1, 2, 2);
    printm_float((double *)m2, 2, 2);
    return 0;
}
#endif
