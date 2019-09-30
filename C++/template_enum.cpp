#include <iostream>
using namespace std;

enum ptS_TIME
  {
    ptSHORT,
    ptMEDIUM,
    ptLONG
  };

int stringToInt(string a)
{
  return (atoi(a.c_str()));
}

template < class T >
T convertString (string sStr)
{ 
  T value;
  

  if      ( sStr == "ptSHORT"  ) value =  ptSHORT;
  else if ( sStr == "ptMEDIUM" ) value =  ptMEDIUM;
  else if ( sStr == "ptLONG"   ) value =  ptLONG;
  else
    value = stringToInt(sStr);

  return value;
}

void printValue(string sStr)
{
  cout << sStr << endl;
}


void printValue(ptS_TIME sStr)
{
  cout << sStr << endl;
}


int main()
{

  string sstr = "ptSHORT";
  printValue(<ptS_TIME>convertString(sstr));

  sstr = "200.5";
  printValue(<int>convertString(sstr));

  return 0;
}
