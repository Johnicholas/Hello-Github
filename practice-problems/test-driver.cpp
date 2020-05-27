#include <iostream>
#include <string>
#include <vector>
#include "people.h"

int main() {
  std::vector<std::vector<std::string>> input {
    {"leetcode","google","facebook"},
    {"google","microsoft"},
    {"google","facebook"},
    {"google"},
    {"amazon"}
  };
  Solution s;
  std::vector<int> output = s.peopleIndexes(input);

  bool first = true;
  for (const auto& i : output) {
    if (first) {
      std::cout << "[";
      first = false;
    } else {
      std::cout << ", ";
    }
    std::cout << i;
  }
  std::cout << "]\n";

  return 0;
}
