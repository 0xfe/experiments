#include <algorithm>
#include <iterator>
#include <iostream>
#include <string>

// build with:
//    g++ -o istream istream.cc

using namespace std;

int main(int argc, char** argv) {
  copy(istream_iterator<string>(cin),
       istream_iterator<string>(),
       ostream_iterator<string>(cout));

  return 0;
}
