#include "people.h"

#include <iostream>
#include <string>
#include <vector>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

using ::testing::ElementsAre;

TEST(FooTest, CanWork) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{"google","facebook"},
	  {"google"},
	    {"amazon"}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1, 4));
}

