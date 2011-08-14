extern "C" {
  #include "table.h"
}

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

int main(int argc, char* argv[]) {
  std::string line;
  while (std::getline(std::cin, line)) {
    std::stringstream line_stream(line);
    std::string command_word;
    if (not (line_stream >> command_word)) {
      continue;
    }
    if (command_word == "lookup") {
      std::string key;
      assert(line_stream >> key);
      char answer[MAX_DEFINITION];
      if (lookup(key.c_str(), answer) == YES) {
	std::cout << "YES, " << answer << "\n";
      } else {
	std::cout << "NO\n";
      }
    } else if (command_word == "install") {
      std::string key;
      std::string value;
      assert(line_stream >> key >> value);
      install(key.c_str(), value.c_str());
    } else {
      std::cout << "I didn't understand that.\n";
    }
  }
  return 0;
}
