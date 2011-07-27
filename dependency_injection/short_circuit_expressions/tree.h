#ifndef TREE_H
#define TREE_H

// forward declaration.
class Context;

// An interface.
class Tree {
public:
  // This is the Non-Virtual Interface idiom or Template Method pattern.
  bool Evaluate(const Context& context) const {
    return this->TreeEvaluate(context);
  }

protected:
  // Constructor.
  Tree() {}

  // Destructor.
  // People shouldn't be deleting trees polymorphically.
  ~Tree() {}

private:
  virtual bool TreeEvaluate(const Context& context) const= 0;

};

#endif // TREE_H

  
