#include "globdefs.h"
#include "common_defio.h"

// get a (possibly pushed back) character
char ngetc(char c) {
  if (bp > 0) {
    c= buf[bp];
  } else {
    bp= 1;
    buf[bp]= getc(c);
  }
  if (c != EOF) {
    --bp;
  }
  return c;
}

