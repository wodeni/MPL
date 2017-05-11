int foo(int a, boolean b, int c) { 
	return 1;
}

int bar(int a, void b, int c) {
	return 1;
} /* Error: illegal void formal b */

int main()
{
  return 0;
}
