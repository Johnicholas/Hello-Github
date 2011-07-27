// Johnicholas Hines January 18 2007
// A singleton language contains exactly one string.
#ifndef SINGLETON_LANGUAGE_H
#define SINGLETON_LANGUAGE_H

#include <string>
#include "language_implementation.h"

using namespace std;

class singleton_language : public language_implementation {
 public:
  singleton_language(string s_init);
  num compute_count(int size);
  void unrank(ostream& out, num rank);
 private:
  string s;
};

#endif // SINGLETON_LANGUAGE_H
