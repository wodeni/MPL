#include <stdlib.h>
#include <stdio.h>

int main(){
	int m[9]={1,2,3,4,5,6,7,8,9};
	FILE* fd = fopen("./matexample.txt","w+");
	int *k;
	k = m;
	int count=0;
	while(count<9){
		count++;
		
		fwrite(k,1,4,fd);
		if(count!=9){
			k++;
		}
	}
	fclose(fd);
	return 0;
}
