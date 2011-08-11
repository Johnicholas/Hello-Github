#include "look.h"

#include "globdefs.h"
#include "common_look.h"
#include <string.h>
#include <stdio.h>

int lookup(char* name, char* defn) {
  int i;
  int j;
  int k;

  for (i= lastp; i > 0; --i) {
    j= namptr[i];
    for (k= 1; name[k] == table[j] && name[k] != EOS; ++k) {
      ++j;
    }
    if (name[k] == table[j]) { // got one
      // scopy(table, j+1, defn, 1); // what was this supposed to do?
      strcpy(table+j+1, defn);
      return YES;
    }
  }
  return NO;
}

void instal(char* name, char* defn) {
  int dlen;
  int nlen;

  nlen= strlen(name) + 1;
  dlen= strlen(defn) + 1;
  if (lastt + nlen + dlen > MAXTBL || lastp >= MAXPTR) {
    fprintf(stderr, "%s: too many definitions.", name);
  }
  ++lastp;
  namptr[lastp]= lastt + 1;
  // scopy(name, 1, table, lastt + 1); // what was this supposed to do?
  strcpy(name, table+lastt+1);
  // scopy(defn, 1, table, lastt + nlen + 1); // what was this supposed to do
  strcpy(defn, table+lastt+nlen+1);
  lastt= lastt + nlen + dlen;
}
       
