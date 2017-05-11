float foo()
{
  if (true) return 42; /* Should return void */
  else return 1;
}

int main()
{
  return 42;
}
