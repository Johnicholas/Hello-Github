// Johnicholas Hines January 18 2007

#include <cassert>

#include "language.h"
#include "language_interface.h"
#include "singleton_language.h"
#include "or_language.h"
#include "and_language.h"


language::language() : implementation(NULL) {
}

void language::initialize_as_singleton(string s) {
  assert( implementation == NULL );
  implementation= new singleton_language(s);
}

void language::initialize_as_and(language* left, language* right) {
  assert( implementation == NULL );
  implementation= new and_language(left, right);
}

void language::initialize_as_or(language* left, language* right) {
  assert( implementation == NULL );
  implementation= new or_language(left, right);
}

void language::initialize_as_language(language* actual) {
  assert( implementation == NULL );
  implementation= actual;
}

num language::count(int size) {
  assert( implementation != NULL );
  // cerr << "language called with count, delegating to implementation" << endl;
  return implementation->count(size);
}

num language::count_below(int size) {
  assert( implementation != NULL );
  return implementation->count_below(size);
}

void language::unrank(ostream& out, num rank) {
  assert( implementation != NULL );
  implementation->unrank(out, rank);
}
