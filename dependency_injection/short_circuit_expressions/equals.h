#ifndef EQUALS_H
#define EQUALS_H

#include "tree.h"
#include <string>

class Equals : public Tree {
public:
  // Constructor.
  Equals(std::string key, std::string value);

  // Accessor - from Tree.
  bool TreeEvaluate(const Context& context) const;

private:
  std::string key;
  std::string value;

};

#endif // EQUALS_H

