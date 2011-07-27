#include "isset.h"
#include "context.h"

// Constructor.
IsSet::IsSet(std::string key) :
  key(key)
{
}

bool IsSet::TreeEvaluate(const Context& context) const {
  return context.IsSet(key);
}

