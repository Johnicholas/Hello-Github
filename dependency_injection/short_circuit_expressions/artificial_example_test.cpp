#include "tree.h"
#include "and.h"
#include "or.h"
#include "isset.h"
#include "equals.h"
#include "question_context.h"
#include <iostream>

int main(int argc, char* argv[]) {
  class ArtificialExampleWiring : public Tree {
  public:
    //! Constructor.
    ArtificialExampleWiring() :
      all(),
      x1("alpha"),
      x2("beta"),
      x3("gamma"),
      atleastone(),
      x4("alpha", "epsilon"),
      x5("beta", "epsilon"),
      x6("gamma", "epsilon")
    {
      all.Add(x1);
      all.Add(x2);
      all.Add(x3);
      atleastone.Add(all);
      atleastone.Add(x4);
      atleastone.Add(x5);
      atleastone.Add(x6);
    }
    
    //! From Tree.
    bool TreeEvaluate(const Context& context) const {
      return this->atleastone.Evaluate(context);
    }
    
  private:
    And all;
    IsSet x1;
    IsSet x2;
    IsSet x3;
    Or atleastone;
    Equals x4;
    Equals x5;
    Equals x6;
  };
  ArtificialExampleWiring the_tree;
  QuestionContext the_context;
  while (true) {
    if (the_tree.Evaluate(the_context)) {
      std::cout << "Result is true.\n";
    } else {
      std::cout << "Result is false.\n";
    }
  }
  return 0;
}

