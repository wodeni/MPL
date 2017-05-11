int foo() { 
	return 1;
}

int bar(int a) { 
	return 1;
} /* Error: entry function shouldn not take any parameters */

int main()
{
  return 0;
}
