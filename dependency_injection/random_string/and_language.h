// Johnicholas Hines January 22 2007
// An "and" language is my term for the concatenation of a pair of languages
#ifndef AND_LANGUAGE_H
#define AND_LANGUAGE_H

#include <string>
#include "language.h"
#include "language_implementation.h"

using namespace std;

class and_language : public language_implementation {
 public:
  and_language(language* left, language* right);
  num compute_count(int size);
  void unrank(ostream& out, num rank);
 private:
  language* left;
  language* right;
};

#endif // AND_LANGUAGE_H
