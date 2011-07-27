#ifndef AND_H
#define AND_H

#include "tree.h"
#include <vector>

class And : public Tree {
public:
  // Mutator.
  void Add(Tree& to_add);

  // Accessor - from Tree.
  bool TreeEvaluate(const Context& context) const;

private:
  std::vector<Tree*> children;

};

#endif // AND_H

