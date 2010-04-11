#include "boost/bind.hpp"
#include "boost/function.hpp"
#include <string>
#include <iostream>

using namespace std;
using boost::bind;
using boost::function;

void FreeFunction(int num1, int num2) {
  cout << "Free function: " << num1 << ", " << num2 << endl;
}

int FreeWithReturn(int num) {
  cout << "Free with return: " << num << endl;
  return num + 1;
}

class MyClass {
 public:
  void MemberFunction(int num) const {
    cout << "Member function: " << num << endl;
  }
};

class StateFunction {
 public:
  void operator() (int i) {
    static bool first_time = true;

    if (first_time) {
      cout << "StateFunction: No state. Setting to: " << i << "." << endl;
      first_time = false;
    } else {
      cout << "StateFunction: Last time was " << state_ << ". This time it's "
           << i << "." << endl;
    }

    state_ = i;
  }

 private:
  int state_;
};

int main() {
  // Free functions
  cout << "Running ordered function" << endl;
  function<void (int, int)> ordered_f = &FreeFunction;
  ordered_f(1, 2);

  cout << "Running unordered function (bind)" << endl;
  function<void (int, int)> unordered_f = bind(&FreeFunction, _2, _1);
  unordered_f(1, 2);

  int returned = (bind<int>(&FreeWithReturn, _1)) (45);
  cout << "Returned: " << returned << endl;

  // Class for member functions
  MyClass me;

  // Pass by value
  function<void (MyClass, int)> member_f = &MyClass::MemberFunction;
  member_f(me, 50);

  // Pass by reference
  function<void (MyClass&, int)> ref_member_f = &MyClass::MemberFunction;
  ref_member_f(me, 50);

  // Pass by pointer
  function<void (MyClass*, int)> ptr_member_f = &MyClass::MemberFunction;
  ptr_member_f(&me, 50);

  // Stateful functions.
  function<void (int)> state_f = StateFunction();
  state_f(10);
  state_f(20);
}
