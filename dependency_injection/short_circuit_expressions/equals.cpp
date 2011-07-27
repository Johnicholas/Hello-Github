#include "equals.h"
#include "context.h"

// Constructor.
Equals::Equals(std::string key, std::string value) :
  key(key),
  value(value)
{
}

bool Equals::TreeEvaluate(const Context& context) const {
  return context.Equals(this->key, this->value);
}

