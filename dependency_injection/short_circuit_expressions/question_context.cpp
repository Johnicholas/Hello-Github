#include "question_context.h"
#include <cassert>
#include <iostream>

bool QuestionContext::ContextIsSet(std::string key) const {
  std::cout << "Is " << key << " set? ";
  std::string line;
  assert(getline(std::cin, line));
  return line == "yes" or line == "y";
}

bool QuestionContext::ContextEquals(std::string key, std::string value) const {
  std::cout << "Is " << key << " set to " << value << "? ";
  std::string line;
  assert(getline(std::cin, line));
  return line == "yes" or line == "y";
}

bool QuestionContext::ContextAction(std::string action) const {
  std::cout << action << "\n";
  std::cout << "Was that successful? ";
  std::string line;
  assert(getline(std::cin, line));
  return line == "yes" or line == "y";
}

