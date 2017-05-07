int foo()
{
	Mat<int> [3][3] m;
	m = [2,2,2;2,2,2;2,2,2];
	bar @ m; 
	printm(m);
	return 1;
}

int bar()
{
	return 1;
}

int main()
{
	Mat<int> [3][3] k;
	k = [1,1,1;1,1,1;1,1,1];
	foo @ k;
	return 1;
}
