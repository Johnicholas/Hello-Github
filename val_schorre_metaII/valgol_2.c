/* An attempt at a Valgol 2 bytecode interpreter */
/* adapting Lennart Augustsson's august.c */

int m[60000];

enum {
  LD,
  LDL,
  SET,
  RST,
  ST,
  ADS,
  SST,
  RSR,
  ADD,
  SUB,
  MLT,
  DIV,
  NEG,
  WHL,
  NOT,
  LEQ,
  LES,
  EQU,
  B,
  BT,
  BF,
  BTP,
  BFP,
  CLL,
  LDF,
  R,
  AIA,
  FLP,
  POP,
  EDT,
  PNT,
  EJT,
  RED,
  WRT,
  HLT
};

int main() {
  /* used in loading to iterate across memory */
  int* q;
  /* the count of bytecodes */
  int c;
  /* the stack pointer */
  int s;
  /* a temporary used in implementing opcodes */
  int r;
  /* the operator */
  int o;
  /* the operator's argument */
  int l;
  /* the program counter */
  /* used in loading to count up to c */
  /* used in the main loop as the program counter */
  int p;
  /* the register */
  int stack1;
  /* the flag register */
  int flag;

  /* load the bytecode into memory */
  q= m;
  /* The length is stored in the first two bytes */
  c= getchar();
  c= c + getchar() * 256;
  p= c;
  while (p--) {
    *q++= getchar();
  }

  s= 60000;
  p= 0;
  while (1) {
    o= m[p++];
    if (o == SET) {
      /* put the integer 1 on top of the stack */
      s--;
      m[s]= 1;
    } else if (o == RST) {
      /* put the integer 0 on top of the stack */
      s--;
      m[s]= 0;
    } else if (o == ST) {
      /* Store the contents of the register, Stack1, in */
      /* the address that is on top of the stack, then */
      /* pop the stack */
      m[m[s]]= stack1;
      s++;
    } else if (o == SST) {

    } else if (o == RSR) {
      s--;
      m[s]= stack1;
    } else if (o == ADD) {
      r= m[s++]; /* pop into r */
      m[s]= m[s] + r; /* add r to new top */
    } else if (o == SUB) {
      r= m[s++]; /* pop into r */
      m[s]= m[s] - r; /* subtract r from new top */
    } else if (o == MLT) {
      r= m[s++];
      m[s]= m[s] * r;
    } else if (o == DIV) {
      r= m[s++];
      m[s]= m[s] / r;
    } else if (o == NEG) {
      m[s]= - m[s];
    } else if (o == WHL) {
      /* TODO truncate the number on top of the stack? */
    } else if (o == NOT) {
      m[s]= ! m[s];
    } else if (o == LEQ) {
      r= m[s++];
      m[s]= m[s] <= r;
    } else if (o == LES) {
      r= m[s++];
      m[s]= m[s] < r;
    } else if (o == EQU) {
      r= m[s++];
      m[s]= m[s] == r;
    } else if (o == CLL) {
      /* Enter a procedure at the address below the flag.. */
    } else if (o == LDF) {
      /* Put the address in the flag register on top of */
      /* the stack, and put the address of the top of the */
      /* stack into the flag register. */
      r= m[s];
      m[s]= flag;
      flag= r;
    } else if (o == R) {
      /* return */
    } else if (o == FLP) {
      r= m[s];
      m[s]= m[s+1];
      m[s+1]= r;
    } else if (o == POP) {
      s++;
    } else if (o == PNT) {
      /* print a line, then space and clear the print area? */
    } else if (o == EJT) {
      /* Position the paper in the printer to the top */
      /* of the next page */
    } else if (o == RED) {
      /* read the first N numbers from a card and store */
      /* them beginning in the address which is next to */
      /* the top of the stack. Pop out both the address */
      /* and the integer. Cards are punched with up to */
      /* 10 eight-digit numbers. Decimal point is assumed */
      /* to be in the middle. An 11-punch over the */
      /* rightmost digit indicates a negative number. */
    } else if (o == WRT) {
      /* Print a line of N numbers beginning in the */
      /* address which is next to the top of the stack */
      /* The integer N is the top term of the stack. */
      /* pop out both the address and the integer. */
      /* twelve character positions are allowed for each */
      /* number. There are four digits before and four */
      /* digits after the decimal. Leading zeroes in front */
      /* of the decimal are changed to blanks. */
    } else if (o == HLT) {
      return;
    } else {
      /* operators with arguments */
      l= m[p++];
      l= l + m[p++] * 256;
      if (o == LD) { /* load an address */
	s--;
	m[s]= l;
      } else if (o == LDL) { /* load a number */
	s--;
	m[s]= l;
      } else if (o == B) {
	p= l;
      } else if (o == BT) {
	if (m[s]) { p= l; }
      } else if (o == BF) {
	if (! m[s]) { p= l; }
      } else if (o == BTP) {
	if (m[s++]) { p= l; }
      } else if (o == BFP) {
	if (! m[s++]) { p= l; }
      } else if (o == EDT) {
	/* Round the number that is on top of the stack */
	/* to the nearest integer N. Move the given string */
	/* into the print area so that its first character */
	/* falls on print position N. */
	/* In case this would cause characters to fall */
	/* outside the print area, no movement takes place */
      }
    }
  }
}



      
