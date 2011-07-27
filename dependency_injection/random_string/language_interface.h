// Johnicholas Hines January 18 2007
// This is (in principle) non-public - people using my context-free-grammar
// system should not need to look at this document.
// It is an interface, though memoization code may go here at some point.
#ifndef LANGUAGE_INTERFACE_H
#define LANGUAGE_INTERFACE_H

#include <string>
#include "num.h"

using namespace std;

class language_interface {
 public:
  virtual num count(int size) = 0;
  virtual num count_below(int size) = 0;
  virtual void unrank(ostream& out, num rank) = 0;
};

#endif // LANGUAGE_INTERFACE_H
