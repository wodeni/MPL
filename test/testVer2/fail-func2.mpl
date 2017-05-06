int foo(int a, boolean b, int c) { 
	return 1;
}

int bar(int a, boolean b, int a) { 
	return 1;
} /* Error: duplicate formal a in bar */

int main()
{
  return 0;
}
