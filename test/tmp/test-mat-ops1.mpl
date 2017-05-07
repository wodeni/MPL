int main()
{
  Mat<int>[2][2] m;
  Mat<int>[2][2] n;
  Mat<int>[2][2] p;
  m = [1,2;3,4];
  n = [5,6;7,8];
  p = n-m;
  printm(p);
  return 0;
}
