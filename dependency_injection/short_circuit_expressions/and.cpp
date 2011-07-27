#include "and.h"

void And::Add(Tree& to_add) {
  children.push_back(&to_add);
}

bool And::TreeEvaluate(const Context& context) const {
  for (unsigned i= 0; i < children.size(); ++i) {
    if (not children[i]->Evaluate(context)) {
      return false;
    }
  }
  return true;
}


