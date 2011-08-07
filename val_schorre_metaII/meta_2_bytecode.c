// an attempt to increase my (Johnicholas's)
// understanding of meta_2_in_meta_2
// lots of lexing and parsing code stolen from Augustsson's parse.oc
#include <assert.h>
#include <stdio.h>

// types
enum token {
  TOK_STRING, // a string
  TOK_ID, // an identifier
  TOK_NUM, // a number

  // single-character tokens

  TOK_OPEN_PAREN, // "("
  TOK_CLOSE_PAREN, // ")"
  TOK_SLASH, // "/"
  TOK_EQUALS, // "="
  TOK_SEMICOLON, // ";"
  TOK_STAR, // "*"
  TOK_STAR_1, // "*1"
  TOK_STAR_2, // "*2"

  // keywords

  TOK_OUT, // ".OUT"
  TOK_LABEL, // ".LABEL"
  TOK_ID_LITERALLY, // ".ID"
  TOK_NUMBER_LITERALLY, // ".NUMBER"
  TOK_STRING_LITERALLY, // ".STRING"
  TOK_EMPTY_LITERALLY, // ".EMPTY"
  TOK_REPEAT, // "\\$"
  TOK_SYNTAX, // ".SYNTAX"
  TOK_END // ".END"
};

/* the upcoming character, used for peeking ahead */
int the_char;
/* the upcoming token, used for peeking the token stream */
enum token the_token;
/* the value of the upcoming token */
int the_lexval;
/* the number of symbols in the names array */
int nsym;
/* the size of the current string */
int strsize;
/* strings and symbols may not be longer than 5000 chars */
char symbol[5000];
/* a sequence of null-terminated strings */
char names[5000];
/* whether the previously-executed test matched */
int match;
/* the value of the lexed token, a number or string index */
int lexval;
/* used for unique values */
int gensym;

int digit(int c) {
  // return (('/' < c) * (c < ':'));
  return isdigit(c);
}

int letter(int c) {
  // return (('`' < c) * (c < '{'));
  // TODO: refactor
  return isalnum(c);
}

/* tests whether p and q are equal null-terminated strings */
int eqstr(char* p, char* q) {
  while (*p) {
    if (*p++ != *q++) {
      return 0;
    }
  }
  return !*q;
}

/* returns index of name within names */
/* installs name if it wasn't already there */
int lookup(char* name) {
  int i;
  char* ns;

  ns= names;
  i= 0;
  while (i < nsym) {
    if (eqstr(ns, name)) {
      /* found it */
      return i;
    }
    /* skip ns forward to the next name */
    while (*ns++)
      ;
    i++;
  }
  /* not found */
  /* copy name into the names array */
  while (*ns++ = *name++)
    ;
  /* increment nsym and return the previous value */
  /* that is, the index of the newly-added name */
  return nsym++;
}

/* returns index of name within names */
/* installs name if it wasn't already there */
char* lookup_by_index(int to_lookup) {
  int i;
  char* ns;

  ns= names;
  i= 0;
  while (i < to_lookup) {
    /* skip ns forward to the next name */
    while (*ns++)
      ;
    i++;
  }
  return ns;
}

/* pulls a character from the character stream */
/* keeps track of one ahead for peeking */
int next() {
  int r;

  r= the_char;
  the_char= getchar();
  return r;
}

/* parse a string from the input (end is a single quote) */
/* the initial quote is already consumed */
int getstring() {
  int c;

  strsize= 0;
  while ((c= next()) != '\'') {
    if (c == '\\') {
      c= next();
      if (c == 'n') c= '\n';
      if (c == 't') c= '\t';
      if (c == '0') c= 0;
      /* note that \\ and \" and so on are handled too */
    }
    symbol[strsize++]= c;
  }
  symbol[strsize++]= 0;
}

/* lexes a token from the character stream */
int getlex() {
  int c;
  char* p;

  /* skip whitespace */
  // while ( ((0 < (c = next())) * (c < '!')) )
  while (isspace(c= next())) {
    ;
  }
  if (c == -1) {
    return c;
  }
  if (c == '(') {
    fprintf(stderr, "getlex returning TOK_OPEN_PAREN\n");
    return TOK_OPEN_PAREN;
  }
  if (c == ')') {
    fprintf(stderr, "getlex returning TOK_CLOSE_PAREN\n");
    return TOK_CLOSE_PAREN;
  }
  if (c == '/') {
    fprintf(stderr, "getlex returning TOK_SLASH\n");
    return TOK_SLASH;
  }
  if (c == '=') {
    fprintf(stderr, "getlex returning TOK_EQUALS\n");
    return TOK_EQUALS;
  }
  if (c == ';') {
    fprintf(stderr, "getlex returning TOK_SEMICOLON\n");
    return TOK_SEMICOLON;
  }
  if (c == '*') {
    /* could be star, star1 or star2 */
    if (the_char == '1') {
      next();
      fprintf(stderr, "getlex returning TOK_STAR_1\n");
      return TOK_STAR_1;
    } else if (the_char == '2') { 
      next();
      fprintf(stderr, "getlex returning TOK_STAR_2\n");
      return TOK_STAR_2;
    } else {
      fprintf(stderr, "getlex returning TOK_STAR\n");
      return TOK_STAR;
    }
  }
  if (c == '\'') {
    /* a string literal */
    getstring();
    the_lexval= lookup(symbol);
    fprintf(stderr, "getlex returning TOK_STRING\n");
    return TOK_STRING;
  }
  if (digit(c)) {
    /* a number literal */
    /* inline itoa */
    the_lexval= c - '0';
    while (digit(the_char)) {
      the_lexval= the_lexval * 10 + next() - '0';
    }
    fprintf(stderr, "getlex returning TOK_NUM\n");
    return TOK_NUM;
  }
  /* default, it's either a keyword or identifier */
  p= symbol;
  *p++= c;
  do {
    *p++= next();
  } while (letter(the_char));
  *p= 0;
  if ((the_lexval= lookup(symbol)) < 9) {
    /* it's a keyword */
    fprintf(stderr, "getlex returning a keyword, %s\n", symbol);
    return the_lexval + 11; // TODO: explain magic number 11
  } else {
    fprintf(stderr, "getlex returning TOK_ID, %s\n", symbol);
    return TOK_ID;
  }
}

int test(enum token to_test) {
  fprintf(stderr, "testing %d against %d\n", the_token, to_test);
  if (the_token == to_test) {
    lexval= the_lexval;
    the_token= getlex();
    match= 1;
  } else {
    match= 0;
  }
  return match;
}

int out1() {
  if (test(TOK_STAR_1)) {
    printf("  GN1\n");
  } else if (test(TOK_STAR_2)) {
    printf("  GN2\n");
  } else if (test(TOK_STAR)) {
    printf("  CI\n");
  } else if (test(TOK_STRING)) {
    printf("  CL '%s'\n", lookup_by_index(lexval));
  }
  return match;
}

int output() {
  if (test(TOK_OUT)) {
    assert(test(TOK_OPEN_PAREN));
    while (out1()) {
      ;
    }
    match= 1;
    assert(test(TOK_CLOSE_PAREN));
  } else if (test(TOK_LABEL)) {
    printf("  LB\n");
    assert(out1());
  }
  printf("  OUT\n");
  return match;
}

int ex3() {
  int spot1;

  if (test(TOK_ID)) {
    printf("  CLL L%s\n", lookup_by_index(lexval));
  } else if (test(TOK_STRING)) {
    /* TODO: should this print the quoted string? */
    printf("  TEST '%s'\n", lookup_by_index(lexval));
  } else if (test(TOK_ID_LITERALLY)) {
    printf("  ID\n");
  } else if (test(TOK_NUMBER_LITERALLY)) {
    printf("  NUM\n");
  } else if (test(TOK_STRING_LITERALLY)) {
    printf("  SR\n");
  } else if (test(TOK_OPEN_PAREN)) {
    assert(ex1());
    assert(test(TOK_CLOSE_PAREN));
  } else if (test(TOK_EMPTY_LITERALLY)) {
    printf("  SET\n");
  } else if (test(TOK_REPEAT)) {
    spot1= gensym++;
    printf("L%d:\n", spot1);
    assert(ex3());
    printf("  BT L%d\n", spot1);
    printf("  SET\n");
  }
  return match;
}

int ex2() {
  int spot1;

  spot1= gensym++;
  if (ex3()) {
    printf("  BF L%d\n", spot1);
  } else {
    output();
  }
  if (match) {
    do {
      if (ex3()) {
	printf("  BE\n");
      } else {
	output();
      }
    } while (match);
    match= 1;
  }
  printf("L%d:\n", spot1);
  return match;
}  

int ex1() {
  int spot1;

  spot1= gensym++;

  if (ex2()) {
    do {
      if (test(TOK_SLASH)) {
	printf("  BT L%d\n", spot1);
	assert(ex2());
      }
    } while (match);
    match= 1;
    printf("L%d:\n", spot1);
  }
  return match;
}
 
int st() {
  if (test(TOK_ID)) {
    printf("L%s:\n", lookup_by_index(lexval));
    assert(test(TOK_EQUALS));
    assert(ex1());
    assert(test(TOK_SEMICOLON));
    printf("  R\n");
  }
  return match;
}

int program() {
  fprintf(stderr, "program %d\n", __LINE__);
  if (test(TOK_SYNTAX)) {
    fprintf(stderr, "program %d\n", __LINE__);
    assert(test(TOK_ID));
    printf("  ADR %s\n", lookup_by_index(lexval));
    do {
      st();
    } while (match);
    match= 1;
    assert(test(TOK_END));
    printf("  END\n");
  }
  return match;
}

int main() {
  int n;
  char* p;
  char* q;

  /* there will be nine keywords */
  nsym= 9;
  /* load the keywords into the names table */
  p= names;
  q= ".OUT\0.LABEL\0.ID\0.NUMBER\0.STRING\0.EMPTY\0.REPEAT\0.SYNTAX\0.END";
  n= sizeof(".OUT\0.LABEL\0.ID\0.NUMBER\0.STRING\0.EMPTY\0.REPEAT\0.SYNTAX\0.END");
  do {
    *p++= *q++;
  } while (n--);
  /* initialize the peek-a-character system */
  the_char= getchar();
  /* initialize the gensym system */
  gensym= 0;
  the_token= getlex();
  assert(program());
  
  return 0;
}

   
