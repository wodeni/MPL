#include <stdlib.h>
#include <stdio.h>
int matread_int (char* path, int* mat, int row, int col){
	FILE* fd = fopen(path,"r");
	if(fd==NULL){
		fclose(fd);
		return -1;
	}
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
int matread_float (char* path, double* mat, int row, int col){
	FILE* fd = fopen(path,"r");
	if(fd==NULL){
		fclose(fd);
		return -1;
	}
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
	return -2;	
}
