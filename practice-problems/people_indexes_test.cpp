#include "people.h"

#include <iostream>
#include <string>
#include <vector>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

using ::testing::ElementsAre;

TEST(FooTest, CanWork1) {
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


TEST(FooTest, CanWork2) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{"google","facebook"},
	  {"google"},
	    {}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1));
}


TEST(FooTest, CanWork3) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{"google","facebook"},
	  {"google"}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1));
}

TEST(FooTest, CanWork4) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{"google","facebook"},
	  {}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1));
}


TEST(FooTest, CanWork5) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{"google","facebook"}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1));
}

TEST(FooTest, CanWork6) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{"google"}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1));
}

TEST(FooTest, CanWork7) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"},
	{}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1));
}

TEST(FooTest, CanWork8) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {"google","microsoft"}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0, 1));
}

TEST(FooTest, CanWork9) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
      {}
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0));
}

TEST(FooTest, CanWork10) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0));
}

TEST(FooTest, CanWork11) {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google"},
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0));
}

TEST(FooTest, CanWork12) {
  std::vector<std::vector<std::string>> input {
    {"leetcode"},
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0));
}

TEST(FooTest, CanWork13) {
  std::vector<std::vector<std::string>> input {
    {},
  };
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre(0));
}

TEST(FooTest, CanWork14) {
  std::vector<std::vector<std::string>> input {};
  Solution toTest;
  EXPECT_THAT(toTest.peopleIndexes(input), ElementsAre());
}


