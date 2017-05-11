int foo(int a, boolean b)
{
	return 1;
}

int main()
{
  foo(42, true);
  foo(42, 42); /* Fail: int, not bool */
}
