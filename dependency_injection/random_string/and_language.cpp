// Johnicholas Hines January 22 2007
// An "and" language is my term for the concatenation of a pair of languages

#include <cassert>
#include "and_language.h"

using namespace std;

and_language::and_language(language* l, language* r) : left(l), right(r) {
  assert( l != NULL );
  assert( r != NULL );
}

num and_language::compute_count(int size) {
  assert( size >= 0 );
  num accumulator;
  accumulator.fromstring("0");
  int leftsize= 1;
  while( leftsize < size ) {
    accumulator.addmul(left->count(leftsize), right->count(size - leftsize));
    leftsize++;
  }
  return accumulator;
}

void and_language::unrank(ostream& out, num rank) {
  int size= 0;
  while( rank.cmp(count_below(size)) >= 0 ) {
    size++;
  }
  // At this point, we've gone one too far. 
  // Go back a step.
  size--;
  
  num rank_inside_size_bucket;
  rank_inside_size_bucket.sub(rank, count_below(size));
  
  num accumulator;
  accumulator.fromstring("0");
  int leftsize= 1;
  do {
    num type_bucket_width;
    type_bucket_width.fromstring("0");
    type_bucket_width.addmul(left->count(leftsize), right->count(size - leftsize));
    num tmp;
    tmp.fromstring("0");
    tmp.add(accumulator, type_bucket_width);
    if( rank_inside_size_bucket.cmp(tmp) < 0 ) {
      break;
    }
    accumulator.add(accumulator, type_bucket_width);
    leftsize++;
  } while( true );

  num rank_inside_type_bucket;
  rank_inside_type_bucket.sub(rank_inside_size_bucket, accumulator);
  num left_rank_inside_size_bucket;
  left_rank_inside_size_bucket.div(rank_inside_type_bucket, right->count(size - leftsize));
  num right_rank_inside_size_bucket;
  right_rank_inside_size_bucket.mod(rank_inside_type_bucket, right->count(size - leftsize));
  num left_rank;
  left_rank.add(left->count_below(leftsize), left_rank_inside_size_bucket);
  num right_rank;
  right_rank.add(right->count_below(size - leftsize), right_rank_inside_size_bucket);

  left->unrank(out, left_rank);
  right->unrank(out, right_rank);
}
