#ifndef CONTEXT_H
#define CONTEXT_H

#include <string>

// An interface.
class Context {
public:
  // Returns true if a key has a value of any kind.
  // Non-Virtual Interface idiom or Template Method pattern.
  bool IsSet(std::string key) const {
    return this->ContextIsSet(key);
  }

  // Returns true if a key's value is equal to value.
  // Non-Virtual Interface idiom or Template Method pattern.
  bool Equals(std::string key, std::string value) const {
    return this->ContextEquals(key, value);
  }

  // Returns true if the action succeeds.
  // Non-Virtual Interface idiom or Template Method pattern.
  bool Action(std::string action) const {
    return this->ContextAction(action);
  }

protected:
  // Constructor.
  Context() {}

  // Destructor.
  ~Context() {}

private:

  virtual bool ContextIsSet(std::string key) const = 0;

  virtual bool ContextEquals(std::string key, std::string value) const = 0;

  virtual bool ContextAction(std::string action) const = 0;

};

#endif // CONTEXT_H

