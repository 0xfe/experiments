#include "boost/bind.hpp"
#include <string>
#include <iostream>

using namespace std;
using boost::bind;

class MyClass {
 public:
  void MemberFunction(int num) {
    cout << "Member function: " << num << endl;
  }
};

void FreeFunction(int num) {
  cout << "Free function: " << num << endl;
}

int FreeWithReturn(int num) {
  cout << "Free with return: " << num << endl;
  return num + 1;
}

int main() {
  (bind(&FreeFunction, _1)) (45);

  MyClass me;
  (bind(&MyClass::MemberFunction, _1, _2)) (&me, 50);

  int returned = (bind<int>(&FreeWithReturn, _1)) (45);
  cout << "Returned: " << returned << endl;
}
