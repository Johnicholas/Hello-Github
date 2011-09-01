#include "minsky_register_machine.h"
#include <stdlib.h>
#include <stdio.h>

struct destination {
  int is_done; // actually a boolean
  const char* to_print;
  const char* name;
};

void set_label(const char* label) {
  // TODO
  fprintf(stderr, "set current label: %s\n", label);
}

void emit_jump_to(const struct destination* next) {
  if (next->is_done) {
    // TODO
    fprintf(stderr, "emit code to print %s.\n", next->to_print);
    fprintf(stderr, "emit code to goto done.\n");
  } else {
    // TODO
    fprintf(stderr, "emit code to goto %s.\n", next->name);
  }
}

void inc(const char* reg, const struct destination* next) {
  fprintf(stderr, "emit code to inc %s\n", reg);
  emit_jump_to(next);
}

void dec(const char* reg,
	 const struct destination* nonzero,
	 const struct destination* zero) {
  // TODO
  fprintf(stderr, "emit code to test %s.\n", reg);
  fprintf(stderr, "emit code to decrement %s.\n", reg);
  emit_jump_to(nonzero);
  emit_jump_to(zero);
}

struct destination* make_label(const char* name) {
  struct destination* answer= malloc(sizeof(struct destination));
  answer->is_done= 0; // false
  answer->to_print= "";
  answer->name= name;
  return answer;
}

struct destination* make_printable(const char* to_print) {
  struct destination* answer= malloc(sizeof(struct destination));
  answer->is_done= 1; // true
  answer->to_print= to_print;
  answer->name= "";
  return answer;
}

const char* make_var(const char* varname) {
  // TODO
  fprintf(stderr, "making var: %s\n", varname);
  return varname;
}

void initialize(const char* reg, int value) {
  // TODO
  fprintf(stderr, "initialize %s to %d.\n", reg, value);
}

void done() {
  // TODO
  fprintf(stderr, "include stdio\n");
  fprintf(stderr, "declare the vars\n");
  fprintf(stderr, "open the main function\n");
  fprintf(stderr, "initialize the vars\n");
  fprintf(stderr, "print the body of the function\n");
  fprintf(stderr, "print done\n");
  fprintf(stderr, "report the ending values of the variables\n");
  fprintf(stderr, "close the main function\n");
}

int main() {
  void* parser= (void*)ParseAlloc(malloc);

  // s0 : a - s1 "Ok"
  Parse(parser, ID, "s0");
  Parse(parser, COLON);
  Parse(parser, ID, "a");
  Parse(parser, MINUS);
  Parse(parser, ID, "s1");
  Parse(parser, STRING, "Ok");
  Parse(parser, NEWLINE);

  // s1 : b + s0
  Parse(parser, ID, "s1");
  Parse(parser, COLON);
  Parse(parser, ID, "b");
  Parse(parser, PLUS);
  Parse(parser, ID, "s0");
  Parse(parser, NEWLINE);

  // a=3 b=4
  Parse(parser, ID, "a");
  Parse(parser, EQUALS);
  Parse(parser, NUMBER, 3);
  Parse(parser, ID, "b");
  Parse(parser, EQUALS);
  Parse(parser, NUMBER, 4);

  // EOF
  Parse(parser, 0);
  
  ParseFree(parser, free);
  return 0;
}
