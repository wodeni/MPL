int reset() {
  return 1;
}

int main()
{
  Mat<int>[2][2] k;
  int q;
  k = [1,2;3,4];
  q = k[1][1];
  print(q);
  reset @ k;
  q = k[1][1];
  print(q);
  return 0;
}
