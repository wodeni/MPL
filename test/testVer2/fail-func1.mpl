int foo() {}

int bar() {}

int baz() {}

int bar() {} /* Error: duplicate function bar */

int main()
{
  return 0;
}
