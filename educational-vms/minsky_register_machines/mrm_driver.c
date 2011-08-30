#include "minsky_register_machine.h"
#include <stdlib.h>
#include <stdio.h>

void add_var(const char* varname) {
  // TODO
  fprintf(stderr, "adding var: %s\n", varname);
}

void done() {
  fprintf(stderr, "done\n");
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
