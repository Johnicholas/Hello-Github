#include "gettok.h"

#include "globdefs.h"
#include "defdefs.h"
#include <ctype.h>

// get alphanumeric string or single non-alpha for define
char gettok(char* token, int toksiz) {
  int i;
  char return_value;
  char c;

  for (i= 1; i < toksiz; ++i) {
    c= ngetc(token[i]);
    if (isalpha(c)) {
      return_value= LETTER;
    } else if (isdigit(c)) {
      return_value= DIGIT;
    } else {
      return_value= OTHER;
    }
    if (return_value != LETTER && return_value != DIGIT) {
      break;
    }
  }
  if (i >= toksiz) {
    error("token too long.");
  }
  if (i > 1) { // some alphanum was seen
    putbak(token[i]);
    --i;
    return_value= ALPHA;
  }
  // single character token
  token[i+1]= EOS;
  return return_value;
}
    
