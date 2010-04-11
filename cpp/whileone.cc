#include <iostream>

using namespace std;

int main(int argc, char** argv) {
  long c = 1;

  while (1) {
    if ((++c % 10000000) == 0) {
      printf("%ld\n", c);
    }
  }

  return 0;
}
