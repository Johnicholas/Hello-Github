#ifndef ACTION_H
#define ACTION_H

#include "tree.h"
#include <string>

class Action : public Tree {
public:
  // Constructor.
  explicit Action(std::string action);

  bool TreeEvaluate(const Context& context) const;

private:
  std::string action;

};

#endif // ACTION_H

 
