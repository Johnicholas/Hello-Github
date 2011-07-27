#include <stdio.h> // just for testing

// forward declarations
class ListOfTrees;
class Tree;

class ListOfTreesVisitor {
public:
  virtual void Empty() { Complain("Empty"); }
  virtual void Nonempty(Tree* first, ListOfTrees* rest) { Complain("Nonempty"); }
  virtual void Complain(const char* function_name) {}
protected:
  // Constructor.
  ListOfTreesVisitor() {}
};

class ListOfTrees {
public:
  virtual void Visit(ListOfTreesVisitor&) = 0;

protected:
  // Constructor.
  ListOfTrees() {}
};

class Empty : public ListOfTrees {
public:
  // Constructor.
  Empty()
  {}
  virtual void Visit(ListOfTreesVisitor& v) {
    v.Empty();
  }
private:
};

class Nonempty : public ListOfTrees {
public:
  // Constructor.
  Nonempty(Tree* first, ListOfTrees* rest) :
    first(first),
    rest(rest)
  {}
  virtual void Visit(ListOfTreesVisitor& v) {
    v.Nonempty(first, rest);
  }
private:
  Tree* first;
  ListOfTrees* rest;
};

class TreeVisitor {
public:
  virtual void Leaf(int value) { Complain("Leaf"); }
  virtual void Branch(Tree* left, Tree* right) { Complain("Branch"); }
  virtual void Complain(const char* function_name) {}
protected:
  // Constructor.
  TreeVisitor() {}
};

class Tree {
public:
  virtual void Visit(TreeVisitor&) = 0;

protected:
  // Constructor.
  Tree() {}
};

class Leaf : public Tree {
public:
  // Constructor.
  Leaf(int value) :
    value(value)
  {}
  virtual void Visit(TreeVisitor& v) {
    v.Leaf(value);
  }
private:
  int value;
};

class Branch : public Tree {
public:
  // Constructor.
  Branch(Tree* left, Tree* right) :
    left(left),
    right(right)
  {}
  virtual void Visit(TreeVisitor& v) {
    v.Branch(left, right);
  }
private:
  Tree* left;
  Tree* right;
};
