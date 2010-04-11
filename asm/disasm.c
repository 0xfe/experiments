#include <stdio.h>

int show(const char* msg) {
  printf(msg);
}

int main(int argc, char** argv) {
  show("Hello World\n");
  return 0;
}
