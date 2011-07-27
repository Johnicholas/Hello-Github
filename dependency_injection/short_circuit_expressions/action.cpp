#include "action.h"
#include "context.h"

// Constructor.
Action::Action(std::string action) :
  action(action)
{
}

bool Action::TreeEvaluate(const Context& context) const {
  return context.Action(action);
}

