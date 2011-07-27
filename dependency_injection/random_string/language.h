// Johnicholas Hines January 18 2007
// This is the public interface to my context-free-language system.
// The goal is that a client should be able to use it including only this file.
// One important feature is that the language object must be constructed
// (like other objects), and then initialized, using one of the initialization
// methods.
// It is a checked runtime error to:
//   1. apply an initialization method to an initialized language
//   2. apply a non-initialization method to an uninitialized language
//
// The reason for this initialization is so that one can create loops.
// For example:
//   language a, b, c, d;
//   a.initialize_as_empty();
//   b.initialize_as_singleton("x");
//   c.initialize_as_or(&a, &b);
//   d.initialize_as_and(&c, &d);
// should initialize d to the language consisting of sequences of the letter x.

#ifndef LANGUAGE_H
#define LANGUAGE_H

#include <string>
#include "language_interface.h"

using namespace std;

class language : public language_interface {
 public:
  language();
  void initialize_as_singleton(string s);
  void initialize_as_and(language* left, language* right);
  void initialize_as_or(language* left, language* right);
  void initialize_as_language(language* actual);
  num count(int size);
  num count_below(int size);
  void unrank(ostream &, num rank);
 private:
  language_interface* implementation;
};

#endif
