#include "boost/scoped_ptr.hpp"
#include <string>
#include <iostream>

using namespace std;
using boost::scoped_ptr;

int main() {
  scoped_ptr<string> s(new string("Scoped string."));

  cout << *s << "\n";
}
