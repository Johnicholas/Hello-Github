#ifndef OR_H
#define OR_H

#include "tree.h"
#include <vector>

class Or : public Tree {
public:
  // Mutator.
  void Add(Tree& to_add);
  
  // Accessor - from Tree.t
  bool TreeEvaluate(const Context& context) const;

private:
  std::vector<Tree*> children;

};

#endif // OR_H

