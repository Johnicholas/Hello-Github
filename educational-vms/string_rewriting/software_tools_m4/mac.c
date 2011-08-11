#include "globdefs.h"
#include "defdefs.h"
#include "common_macro.h"
#include "gettok.h"
#include "look.h"
#include <string.h>

#define min(x,y) ((x) < (y) ? (x) : (y))

void putchr(char c) {
  if (cp == 0) {
    putc(c);
  } else {
    evalst[ep++]= c;
  }
  return;
}
// push ep onto argstk, return new pointer ap
int push(int ep, int* argstk, int ap) { 
  argstk[ap++]= ep;
  return ap;
}

// put a token either on output or into evaluation stack
void puttok(char* str) {
  int i;
  for (i= 1; str[i] != EOS; ++i) {
    putchr(str[i]);
  }
}

// convert number to string, push back on input
void pbnum(int n) {
  int m;
  int num;
  char digits[]= "0123456789";

  num= n;
  do {
    m= num % 10;
    putbak(digits[m+1]);
    num= num / 10;
  } while (num != 0);
}

// increment argument by 1
void doincr(int* argstk, int i, int j) {
  int k;

  k= argstk[i+2];
  pbnum(atoi(evalst)+1);
}

void doif(int* argstk, int i, int j) {
  int a2;
  int a3;
  int a4;
  int a5;

  if (j - i < 5) {
    return;
  }
  a2= argstk[i+2];
  a3= argstk[i+3];
  a4= argstk[i+4];
  a5= argstk[i+5];
  if (strcmp(evalst+a2, evalst+a3) == 0) { // subarrays
    pbstr(evalst[a4]);
  } else {
    pbstr(evalst[a5]);
  }
}

// install definition in table
void dodef(int* argstk, int i, int j) {
  int a2;
  int a3;

  if (j - i > 2) {
    a2= argstk[i+2];
    a3= argstk[i+3];
    instal(evalst+a2, evalst+a3); // subarrays
  }
}

// select substring
void dosub(int* argstk, int i, int j) {
  int max;
  int ap;
  int fc;
  int k;
  int nc;

  if (j - i < 3) {
    return;
  }
  if (j - i < 4) {
    nc= MAXTOK;
  } else {
    k= argstk[i+4];
    nc= atoi(evalst); // was ctoi(evalst, k); // number of characters
  }
  k= argstk[i+3]; // origin
  ap= argstk[i+2]; // target string
  // was fc= ap + ctoi(evalst, k) - 1; // first char of substring
  fc= ap + atoi(evalst) - 1; // first char of substring
  if (fc >= ap && fc < ap + strlen(evalst+ap)) { // subarrays
    k= fc + min(nc, strlen(evalst+fc)) - 1;
    for (; k >= fc; k= k - 1) {
      putbak(evalst+k);
    }
  }
}

void eval(int* argstk, int i, int j) {
  int argno;
  int k;
  int m;
  int n;
  int t;
  int td;
  char digits[]= "0123456789";
  
  t= argstk[i];
  td= evalst[t];
  if (td == DEFTYPE) {
    dodef(argstk, i, j);
  } else if (td == INCTYPE) {
    doincr(argstk, i, j);
  } else if (td == SUBTYPE) {
    dosub(argstk, i, j);
  } else if (td == IFTYPE) {
    doif(argstk, i, j);
  } else {
    for (k= t+strlen(evalst+t)-1; k > t; k= k - 1) {
      if (evalst[k-1] != ARGFLAG) {
	putbak(evalst[k]);
      } else {
	argno= (int)(strchr(digits, evalst[k])-digits);
	if (argno >= 0 && argno < j - i) {
	  n= i + argno + 1;
	  m= argstk[n];
	  pbstr(evalst[m]);
	}
	k= k - 1; // skip over $
      }
      if (k == t) { // do last character
	putbak(evalst[k]);
      }
    }
  }
}

int main() {
  char defn[MAXDEF];
  char t;
  char token[MAXTOK];
  int ap;
  int argstk[ARGSIZE];
  int callst[CALLSIZE];
  int nlb;
  int plev[CALLSIZE];
  char balp[]= "()";
  char defnam[]= "define";
  char incnam[]= "incr";
  char subnam[]= "substr";
  char ifnam[]= "ifelse";
  char deftyp[]= { DEFTYPE, EOS };
  char inctyp[]= { INCTYPE, EOS };
  char subtyp[]= { SUBTYPE, EOS };
  char iftyp[]= { IFTYPE, EOS };

  instal(defnam, deftyp);
  instal(incnam, inctyp);
  instal(subnam, subtyp);
  instal(ifnam, iftyp);

  cp= 0;
  ap= 1;
  for (t= gettok(token, MAXTOK); t != EOF; t= gettok(token, MAXTOK)) {
    if (t == ALPHA) {
      if (lookup(token, defn) == NO) {
	puttok(token);
      } else { // it's defined, put it in eval stack
	cp= cp+1;
	if (cp > CALLSIZE) {
	  error("call stack overflow.");
	}
	callst[cp]= ap;
	ap= push(ep, argstk, ap);
	puttok(defn); // stack definition
	putchr(EOS);
	ap= push(ep, argstk, ap);
	puttok(token); // stack name
	putchr(EOS);
	ap= push(ep, argstk, ap);
	t= gettok(token, MAXTOK); // peek at next
	pbstr(token);
	if (t != LPAREN) { // add ( ) if not present
	  pbstr(balp);
	}
	plev[cp]= 0;
      }
    } else if (t == LBRACK) { // strip one level of [ ]
      nlb= 1;
      do {
	t= gettok(token, MAXTOK);
	if (t == LBRACK) {
	  nlb= nlb + 1;
	} else if (t == RBRACK) {
	  nlb= nlb - 1;
	  if (nlb == 0) {
	    break;
	  }
	} else if (t == EOF) {
	  error("EOF in string.");
	} 
	puttok(token);
      } while (1);
    } else if (cp == 0) { // not in a macro at all
      puttok(token);
    } else if (t == LPAREN) {
      if (plev[cp] > 0) {
	puttok(token);
      } 
    } else if (t == RPAREN) {
      plev[cp]= plev[cp] - 1;
      if (plev[cp] > 0) {
	puttok(token);
      } else { // end of argument list
	putchr(EOS);
	eval(argstk, callst[cp], ap-1);
	ap= callst[cp]; // pop eval stack
	ep= argstk[ap];
	cp= cp - 1;
      }
    } else if (t == COMMA && plev[cp] == 1) { // new arg
      putchar(EOS);
      ap= push(ep, argstk, ap);
    } else {
      puttok(token); // just stack it
    }
  }
  if (cp != 0) {
    error("unexpected EOF.");
  }
  return 0;
}

