int foo() {}

int bar() {
  int a;
  void b; /* Error: illegal void local b */
  boolean c;

  return 0;
}

int main()
{
  return 0;
}
