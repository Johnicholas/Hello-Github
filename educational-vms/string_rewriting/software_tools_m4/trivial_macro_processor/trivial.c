#include "io.h"
#include "table.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

enum token_type { ALPHA= 0, NONALPHA= 1 };

/* gets a single non-alphanumeric character, or a maximal sequence of alphanumeric characters */
int get_token(char token[MAX_TOKEN]) {
  int i= 0;
  int c= get_char();
  if (c == EOF) {
    return EOF;
  } else if (isalpha(c)) {
    do {
      token[i++]= c;
    } while (isalnum(c= get_char()));
    put_back(c);
    token[i++]= '\0';
    return ALPHA;
  } else {
    token[i++]= c;
    token[i++]= '\0';
    return NONALPHA;
  }
}

/* we've seen the keyword that tells us a definition is coming up */
/* this function gets the '(foo, bar)' part. */
void get_definition(char token[MAX_TOKEN], char definition[MAX_DEFINITION]) {
  int num_left_parens;
  int i;

  assert(get_char() == '(');
  assert(get_token(token) == ALPHA);
  assert(get_char() == ',');
  for (i= 0, num_left_parens= 0; num_left_parens >= 0; ++i) {
    assert(i < MAX_DEFINITION);
    assert((definition[i]= get_char()) != EOF);
    if (definition[i] == '(') {
      ++num_left_parens;
    } else if (definition[i] == ')') {
      --num_left_parens;
    }
  }
  definition[--i]= '\0';
}

void put_back_string(const char* to_put_back) {
  int i;
  for (i= strlen(to_put_back); i >= 0; --i) {
    put_back(to_put_back[i]);
  }
}
  
int main(int argc, char* argv[]) {
  char definition[MAX_DEFINITION];
  int t;
  char token[MAX_TOKEN];

  install("define", "define");
  for (t= get_token(token); t != EOF; t= get_token(token)) {
    if (t != ALPHA) {
      /* non-alphabetic, print it */
      printf("%s", token);
    } else if (lookup(token, definition) == NO) {
      /* alphabetic, but no corresponding definition, print it. */
      printf("%s", token);
    } else if (strcmp(definition, "define") == 0) {
      /* the primitive macro - define. */
      get_definition(token, definition);
      install(token, definition);
    } else {
      /* the body of the macro, which needs to be re-scanned. */
      put_back_string(definition);
    }
  }
  return 0;
}


