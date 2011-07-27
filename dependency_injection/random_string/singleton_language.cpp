// Johnicholas Hines January 18 2007
// A singleton language contains exactly one string.

#include <cassert>
#include "singleton_language.h"

// #define COUNT_BY_TOKENS

singleton_language::singleton_language(string s_init) : s(s_init) {
}

num singleton_language::compute_count(int size) {
  num result;
#ifdef COUNT_BY_TOKENS
  if( size == 1 ) {
#else
    if( (unsigned)size == s.length() ) {
#endif
    result.fromstring("1");
  } else {
    result.fromstring("0");
  }
  return result;
}

void singleton_language::unrank(ostream& out, num rank) {
  num tmp;
  tmp.fromstring("0");
  assert( rank.cmp(tmp)==0 );
  out << s;
}

