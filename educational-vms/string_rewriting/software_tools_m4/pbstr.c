#include "globdefs.h"
#include <string.h>

void pbstr(char* in) {
  int i;

  for (i= strlen(in); i > 0; --i) {
    putbak(in[i]);
  }
}

