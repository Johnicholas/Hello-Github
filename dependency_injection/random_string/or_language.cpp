// Johnicholas Hines January 19 2007
// An "or" language is my term for binary union.
// It corresponds to the vertical bar "|" in regexp notation.

#include <cassert>
#include "or_language.h"

using namespace std;

or_language::or_language(language* l, language* r) : left(l), right(r) {
  assert( l != NULL );
  assert( r != NULL );
}

num or_language::compute_count(int size) {
  assert( size >= 0 );
  num result;
  result.add(left->count(size), right->count(size));
  return result;
}

void or_language::unrank(ostream& out, num rank) {
  int size= 0;
  while( rank.cmp(count_below(size)) >= 0 ) {
    size++;
  }
  // At this point, we've gone one too far. 
  // Go back a step.
  size--;
  num rank_inside_size_bucket;
  rank_inside_size_bucket.sub(rank, count_below(size));
  num argument;
  if( rank_inside_size_bucket.cmp(left->count(size)) < 0 ) {
    argument.add(left->count_below(size), rank_inside_size_bucket);
    left->unrank(out, argument);
  } else {
    argument.add(right->count_below(size), rank_inside_size_bucket);
    argument.sub(argument, left->count(size));
    right->unrank(out, argument);
  }
}
