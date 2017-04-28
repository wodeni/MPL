int f1(int a, int b){
	return 1;
}

int f2(int a, int b){
	return 2;
}
int main (){
	int (*foo_ptr_array[2])(int, int)={ f1,f2 };
//	foo_ptr_array[1] = { f1,f1 };
	int k = foo_ptr_array[0](3, 3);
	int l = foo_ptr_array[1](3, 3);
	return 0;
}
