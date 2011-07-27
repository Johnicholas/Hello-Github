// Johnicholas Hines Jan 25 2007 
// A header file for a class of numbers implemented as ints

#ifndef NUM_H
#define NUM_H

#include <string>
#include <vector>
#include "xp.h"

using namespace std;

class num {
 public:
  // pre: decimal is a decimal integer
  //      checked runtime error if decimal is badly formatted
  // post: this is the number that decimal represented
  void fromstring(string decimal);
  // post: this is "x + y"
  void add(const num& x, const num& y);
  // post: this is "x - y" unless y is greater than x
  //       if y is greater than x, this is zero
  void sub(const num& x, const num& y);
  // post: this is "this + x * y"
  void addmul(const num& x, const num& y);
  // post: this is "x div y"; that is, integer division 
  //       checked runtime error if y is zero
  void div(const num& x, const num& y);
  // post: this is "x % y"; that is, remainder after integer division
  //       checked runtime error if y is zero
  void mod(const num& x, const num& y);
  // post: result is <0,==0,>0 as this<x,this==x,this>y respectively
  int cmp(const num& x);
  // post: result is the decimal string representation of this number
  string print();  
 private:
  // I'm not hiding the implementation properly at the moment
  // this implementation is used with littlenum.h
  // int implementation;
  // the T implementation is used with bignum.h
  T implementation;
};

#endif // NUM_H
