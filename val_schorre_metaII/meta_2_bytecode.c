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
  TOK_EMPTY, // ".EMPTY"
  TOK_REPEAT, // ".REPEAT"
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
  symbol[strsize++]= '\'';
  while ((c= next()) != '\'') {
    if (c == '\\') {
      symbol[strsize++]= c;
      c= next();
    }
    symbol[strsize++]= c;
  }
  symbol[strsize++]= '\'';
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
    // fprintf(stderr, "getlex returning TOK_OPEN_PAREN\n");
    return TOK_OPEN_PAREN;
  }
  if (c == ')') {
    // fprintf(stderr, "getlex returning TOK_CLOSE_PAREN\n");
    return TOK_CLOSE_PAREN;
  }
  if (c == '/') {
    // fprintf(stderr, "getlex returning TOK_SLASH\n");
    return TOK_SLASH;
  }
  if (c == '=') {
    // fprintf(stderr, "getlex returning TOK_EQUALS\n");
    return TOK_EQUALS;
  }
  if (c == ';') {
    // fprintf(stderr, "getlex returning TOK_SEMICOLON\n");
    return TOK_SEMICOLON;
  }
  if (c == '*') {
    /* could be star, star1 or star2 */
    if (the_char == '1') {
      next();
      // fprintf(stderr, "getlex returning TOK_STAR_1\n");
      return TOK_STAR_1;
    } else if (the_char == '2') { 
      next();
      // fprintf(stderr, "getlex returning TOK_STAR_2\n");
      return TOK_STAR_2;
    } else {
      // fprintf(stderr, "getlex returning TOK_STAR\n");
      return TOK_STAR;
    }
  }
  if (c == '\'') {
    /* a string literal */
    getstring();
    the_lexval= lookup(symbol);
    // fprintf(stderr, "getlex returning TOK_STRING\n");
    return TOK_STRING;
  }
  if (digit(c)) {
    /* a number literal */
    /* inline itoa */
    the_lexval= c - '0';
    while (digit(the_char)) {
      the_lexval= the_lexval * 10 + next() - '0';
    }
    // fprintf(stderr, "getlex returning TOK_NUM\n");
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
    // fprintf(stderr, "getlex returning a keyword, %s\n", symbol);
    return the_lexval + 11; // TODO: explain magic number 11
  } else {
    // fprintf(stderr, "getlex returning TOK_ID, %s\n", symbol);
    return TOK_ID;
  }
}

int tst(enum token to_test) {
  // fprintf(stderr, "testing %d against %d\n", the_token, to_test);
  if (the_token == to_test) {
    lexval= the_lexval;
    the_token= getlex();
    match= 1;
  } else {
    match= 0;
  }
  return match;
}

int indent= 1;
int written= 0;

void do_indent() {
  if (indent) {
    printf("    ");
    indent= 0;
  }
}

void cl(const char* op) {
  do_indent();
  printf("%s ", op);
  written= 1;
}

void out() {
  if (written) {
    printf("\n");
    indent= 1;
    written= 0;
  }
}

void sr() { tst(TOK_STRING); }
void id() { tst(TOK_ID); }
void num() { tst(TOK_NUM); }
void ci() {
  printf("%s", lookup_by_index(lexval));
  written= 1;
}

#define be() assert(match);
void set() { match= 1; }
void lb() { indent= 0; }

void make_label(int label) {
  printf("L%d\n", label);
  indent= 1;
  written= 0;
}
#define gn1() if (label1 == -1) { label1= gensym++; } make_label(label1);
#define gn2() if (label2 == -1) { label2= gensym++; } make_label(label2);
#define bf(label) if (! match) { goto label; }
#define bt(label) if (match) { goto label; }
#define cll(fun) fun()

// forward declarations
void out1();
void output();
void ex1();
void ex2();
void ex3();
void st();
void program();

void out1() {
  int label1= -1;
  int label2= -1;

  tst(TOK_STAR_1);
  bf(L0);
  cl("GN1");
  out();
 L0:
  bt(L1);
  tst(TOK_STAR_2);
  bf(L2);
  cl("GN2");
  out();
 L2:
  bt(L1);
  tst(TOK_STAR);
  bf(L3);
  cl("CI");
  out();
 L3:
  bt(L1);
  sr();
  bf(L4);
  cl("CL");
  ci();
  out();
 L4:
 L1:
  return;
}

void output() {
  int label1= -1;
  int label2= -1;

  tst(TOK_OUT);
  bf(L5);
  tst(TOK_OPEN_PAREN);
  be();
 L6:
  cll(out1);
  bt(L6);
  set();
  be();
  tst(TOK_CLOSE_PAREN);
  be();
 L5:
  bt(L7);
  tst(TOK_LABEL);
  bf(L8);
  cl("LB");
  out();
  cll(out1);
  be();
 L8:
 L7:
  bf(L9);
  cl("OUT");
  out();
 L9:
 L10:
  return;
}

void ex3() {
  int label1= -1;
  int label2= -1;

  id();
  bf(L11);
  cl("CLL");
  ci();
  out();
 L11:
  bt(L12);
  sr();
  bf(L13);
  cl("TST");
  ci();
  out();
 L13:
  bt(L12);
  tst(TOK_ID_LITERALLY);
  bf(L14);
  cl("ID");
  out();
 L14:
  bt(L12);
  tst(TOK_NUMBER_LITERALLY);
  bf(L15);
  cl("NUM");
  out();
 L15:
  bt(L12);
  tst(TOK_STRING_LITERALLY);
  bf(L16);
  cl("SR");
  out();
 L16:
  bt(L12);
  tst(TOK_OPEN_PAREN);
  bf(L17);
  cll(ex1);
  be();
  tst(TOK_CLOSE_PAREN);
  be();
 L17:
  bt(L12);
  tst(TOK_EMPTY);
  bf(L18);
  cl("SET");
  out();
 L18:
  bt(L12);
  tst(TOK_REPEAT);
  bf(L19);
  lb();
  gn1();
  out();
  cll(ex3);
  be();
  cl("BT");
  gn1();
  out();
  cl("SET");
  out();
 L19:
 L12:
  return;
}

void ex2() {
  int label1= -1;
  int label2= -1;
  
  cll(ex3);
  bf(L20);
  cl("BF");
  gn1();
  out();
 L20:
  bt(L21);
  cll(output);
  bf(L22);
 L22:
 L21:
  bf(L23);
 L24:
  cll(ex3);
  bf(L25);
  cl("BE");
  out();
 L25:
  bt(L26);
  cll(output);
  bf(L27);
 L27:
 L26:
  bt(L24);
  set();
  be();
  lb();
  gn1();
  out();
 L23:
 L28:
  return;
}  

void ex1() {
  int label1= -1;
  int label2= -1;

  cll(ex2);
  bf(L29);
 L30:
  tst(TOK_SLASH);
  bf(L31);
  cl("BT");
  gn1();
  out();
  cll(ex2);
  be();
 L31:
 L32:
  bt(L30);
  set();
  be();
  lb();
  gn1();
  out();
 L29:
 L33:
  return;
}
 
void st() {
  int label1= -1;
  int label2= -1;

  id();
  bf(L34);
  lb();
  ci();
  out();
  tst(TOK_EQUALS);
  be();
  cll(ex1);
  be();
  tst(TOK_SEMICOLON);
  be();
  cl("R");
  out();
 L34:
 L35:
  return;
}

void program() {
  int label1= -1;
  int label2= -1;

  tst(TOK_SYNTAX);
  bf(L36);
  id();
  be();
  cl("ADR");
  ci();
  out();
 L37:
  cll(st);
  bt(L37);
  set();
  be();
  tst(TOK_END);
  be();
  cl("END");
  out();
 L36:
 L38:
  return;
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
  program();
  assert(match);
  
  return 0;
}

   
