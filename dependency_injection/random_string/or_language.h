// Johnicholas Hines January 19 2007
// An "or" language is my idiosyncratic terminology for a binary union.
// It corresponds to the vertical bar | in regexp or cfg notation.
#ifndef OR_LANGUAGE_H
#define OR_LANGUAGE_H

#include <string>
#include "language.h"
#include "language_implementation.h"

using namespace std;

class or_language : public language_implementation {
 public:
  or_language(language* left, language* right);
  num compute_count(int size);
  void unrank(ostream& out, num rank);
 private:
  language* left;
  language* right;
};

#endif // OR_LANGUAGE_H
