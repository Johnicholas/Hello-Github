// Johnicholas Hines Jan 25 2007 
// A cpp file for a class of numbers implemented as ints

#include "num.h"

#include <string>
#include <sstream>
#include <iostream> // DEBUG
using namespace std;

// pre: decimal is a decimal integer
//      checked runtime error if decimal is badly formatted
// post: this is the number that decimal represented
void num::fromstring(string decimal) {
  // the second argument, 10, means the string is in base 10
  implementation= AP_fromstr(decimal.c_str(), 10);
}

// post: this is "x + y"
void num::add(const num& x, const num& y) {
  implementation= AP_add(x.implementation, y.implementation);
}

// post: this is "x - y" unless y is greater than x
//       if y is greater than x, this is zero
void num::sub(const num& x, const num& y) {
  implementation= AP_sub(x.implementation, y.implementation);
}

// post: this is "this + x * y"
void num::addmul(const num& x, const num& y) {
  T cache= implementation;
  implementation= AP_mul(x.implementation, y.implementation);
  implementation= AP_add(cache, implementation);
}

// post: this is "x div y"; that is, integer division 
//       checked runtime error if y is zero
void num::div(const num& x, const num& y) {
  implementation= AP_div(x.implementation, y.implementation).first;
}

// post: this is "x % y"; that is, remainder after integer division
//       checked runtime error if y is zero
void num::mod(const num& x, const num& y) {
  implementation= AP_div(x.implementation, y.implementation).second;
}

// post: result is <0,==0,>0 as this<x,this==x,this>x respectively
int num::cmp(const num& x) {
  return AP_cmp(implementation, x.implementation);
}

// post: result is the decimal string representation of this number
string num::print() {
  // the second argument, 10, means we want output in base 10
  return AP_tostr(implementation, 10);
}
