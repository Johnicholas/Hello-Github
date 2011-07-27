#ifndef QUESTION_CONTEXT_H
#define QUESTION_CONTEXT_H

#include "context.h"

// A context that delegates to the console.
class QuestionContext : public Context {
public:
  // From Context.
  bool ContextIsSet(std::string key) const;

  // From Context.
  bool ContextEquals(std::string key, std::string value) const;

  // From Context.
  bool ContextAction(std::string action) const;

};

#endif // QUESTION_CONTEXT_H

