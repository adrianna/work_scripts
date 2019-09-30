#include <iostream>
using namespace std;


template < class T >
T GetMax (T a, T b )
{
  return ( a > b? a:b );
}

template < class T, class U >
T GetMin(T a, U b)
{
  return ( a < b? a:b );
}

template < class T >
class mypair
{
  T values [2];

public:
  mypair(T first, T second)
  {
    values[0] = first; values[1] = second;
  }

}

int main()
{

  int i = 5, j = 6, k;
  long l = 1, m = 5, n;

  k = GetMax(i,j);
  n = GetMax(l,m);

  cout << k << endl;
  cout << n << endl;

  i = GetMin(j, 3);
  cout << i << endl;

  return 0;
}
