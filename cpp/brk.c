#include <unistd.h>
#include <stdio.h>

int main(int argc, char** argv) {
  void *last_valid_address;

  last_valid_address = sbrk(0);

  printf("last_valid_address: %p\n", last_valid_address);
}
  
