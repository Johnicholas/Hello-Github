#include "people-indexes.h"

#include <iostream>
#include <string>
#include <vector>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "golden-people-indexes.h"

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;

TEST(PeopleIndexes, GoldenImplementation) {
  std::vector<std::vector<std::string>> big_input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{"google","facebook"},
	  {"google"},
	    {"amazon"}
  };

  std::vector<std::vector<std::string>> input;
  for (const auto& person : big_input) {
    input.emplace_back();
    for (const auto& company : person) {
      input.back().push_back(company);
      Solution toTest;
      EXPECT_THAT(toTest.peopleIndexes(input), ElementsAreArray(GoldenPeopleIndexes(input)));
    }
  }
}
