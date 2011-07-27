#ifndef ISSET_H
#define ISSET_H

#include "tree.h"
#include <string>

class IsSet : public Tree {
public:
  // Constructor.
  explicit IsSet(std::string key);

  // Accessor - from Tree.
  bool TreeEvaluate(const Context&) const;

private:
  std::string key;

};

#endif // ISSET_H

