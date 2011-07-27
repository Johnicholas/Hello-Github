// Johnicholas Hines January 24 2007
// This class inherits from language_interface.
// It has virtual methods compute_count and unrank.
// Assuming that compute_count and unrank are provided
// by a subclass, it provides memoized implementations
// of count and count_below.

#ifndef LANGUAGE_IMPLEMENTATION_H
#define LANGUAGE_IMPLEMENTATION_H

#include <string>
#include <vector>
#include "language_interface.h"

using namespace std;

class language_implementation : public language_interface {
 public:
  num count(int size) {
    for( int i= memo_count.size(); i<=size; i++ ) {
      memo_count.push_back(this->compute_count(i));
    }
    return memo_count[size];
  };
  /*
  // Johnicholas: This is the un-memoized version of count_below, 
  // kept for speed comparison purposes.
  num count_below(int size) {
    num total;
    total.fromstring("0");
    for( int i= 0; i<size; i++ ) {
      total.add(total, this->count(i));
    }
    return total;
  };
  */
  num count_below(int size) {
    for( int i= memo_count_below.size(); i<=size; i++ ) {
      memo_count_below.push_back(this->compute_count_below(i));
    }
    return memo_count_below[size];
  };
  num compute_count_below(int size) {
    num total;
    if( size > 0 ) {
      total.add(this->count_below(size-1), this->count(size-1));
    } else {
      total.fromstring("0");
    }
    return total;
  };
  virtual void unrank(ostream& out, num rank) = 0;
  virtual num compute_count(int size) = 0;
 private:
  vector<num> memo_count;
  vector<num> memo_count_below;
};

#endif // LANGUAGE_IMPLEMENTATION_H
