int foo(int c)    
{
 	return 1;
}

int main()
{
 	int a;
	foo(a); /*error: not initialize a*/ 
	return 0;
}
