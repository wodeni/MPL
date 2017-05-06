int foo(int a, boolean b)
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
  	foo(42, true);

	return 1;
}
