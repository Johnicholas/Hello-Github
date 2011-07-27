#include "or.h"

void Or::Add(Tree& to_add) {
  children.push_back(&to_add);
}

bool Or::TreeEvaluate(const Context& context) const {
  for (unsigned i= 0; i < children.size(); ++i) {
    if (children[i]->Evaluate(context)) {
      return true;
    }
  }
  return false;
}

