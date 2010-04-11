// Author: mmuthanna@google.com (Mohit Cheppudira)
//
#include <iostream>
#include <string>

int main(int argc, char argv[]) {
  std::string a;

  std::cin >> a;
  std::cout << "1: " << a << std::endl;

  std::cin >> a;
  std::cout << "2: " << a << std::endl;

  if (a.empty()) {
    std::cout << "A is empty." << std::endl;
  }
}
