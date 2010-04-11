// Unit test test.
// Mohit Cheppudira.
//
// Build with (if BOOST_TEST_DYN_LINK is defined):
// $ g++ unittest.cc -lboost_unit_test_framework
//
// or statically:
// $ g++ unittest.cc /usr/lib/libboost_unit_test_framework.a

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>

int add(int i, int j) { return i + j; }

BOOST_AUTO_TEST_SUITE(adder)

BOOST_AUTO_TEST_CASE(case1) {
  // Check for failure
  BOOST_CHECK(add(2, 2) == 4);

  // Carp on failure
  BOOST_REQUIRE(add(2, 2) == 4);
}

BOOST_AUTO_TEST_CASE(case2) {
  // Check for failure
  BOOST_CHECK(add(2, 3) == 4);

  // Carp on failure
  BOOST_REQUIRE(add(2, 2) == 4);
}

BOOST_AUTO_TEST_CASE(case3) {
  // Check for failure
  BOOST_CHECK(add(2, 3) == 4);

  // Carp on failure
  BOOST_REQUIRE(add(2, 2) == 4);
}

BOOST_AUTO_TEST_SUITE_END()
