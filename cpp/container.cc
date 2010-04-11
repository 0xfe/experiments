// Author: mmuthanna@google.com (Mohit Cheppudira)
//
#include <iostream>
#include <string>

using namespace std;

template <typename T>
class Container {
 public:
  T internal;
  static Container<T>* lastInstance;

  Container(T value) {
    internal = value;
    lastInstance = this;
  }
};

int main(int argc, char** argv) {
  Container<string>* str = new Container<string>("abc");
  Container<int>* i = new Container<int>(1);

  cout << str->lastInstance->internal << "\n";
}
