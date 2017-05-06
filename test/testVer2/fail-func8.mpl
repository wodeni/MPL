int foo()
{
	Mat<int> [3][3] m;
	m = [1,1,1;1,1,1;1,1,1];
	bar() @ m; /* entry function can not call another entry function*/
	return 1; 
}

int bar()
{
	return 1;
}

int main()
{
	return 1;
}
