int main()
{
  Mat<int>[2][2] m;
  Mat<int>[2][2] n;
  Mat<int>[2][2] p;
  p = m*n;
  m = [1,2;3,4];
  n = [5,6;7,8];
  printm(m*n);
  return 0;
}
