#include "globdefs.h"
#include "common_defio.h"

void putbak(char c) {
  bp= bp+1;
  if (bp > BUFSIZE) {
    error("too many characters pushed back.");
  }
  buf[bp]= c;
}

