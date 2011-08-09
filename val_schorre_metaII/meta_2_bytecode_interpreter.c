// Lots of code stolen from Augustsson's IOCCC-winning vm
#include <assert.h>
#include <stdio.h>

enum opcodes {
  OP_ID,
  OP_NUM,
  OP_SR,
  OP_R,
  OP_SET,
  OP_BE,
  OP_CI,
  OP_GN1,
  OP_GN2,
  OP_LB,
  OP_OUT,
  OP_TST,
  OP_CLL,
  OP_B,
  OP_BT,
  OP_BF,
  OP_CL,
  OP_ADR,
  OP_STRING
};

int m[60000];

int main() {
  /* things stolen from august.c */
  int pc;
  int* q;
  int count;
  int sp;
  int reg;
  int op;
  int arg;

  /* registers of the meta 2 virtual machine */
  int match; // did the most recent test succeed?
  int written; // has anything been written on the current line?
  int gensym; // used to generate unique symbols
  int indent; // should this line be indented?

  // the length of the bytecode is stored in the first two bytes.
  count= getchar();
  count= count + getchar() * 256;
  // load the opcodes into memory
  pc= count;
  q= m;
  while (pc--) {
    *q++= getchar();
  }

  sp= 60000;
  pc= 0;
  while (0 <= pc) {
    op= m[pc++];
    // unary ops (they might manipulate the stack, but no args in the source)
    if (op == OP_ID) {
      // tst(TOK_ID); // TODO
    } else if (op == OP_NUM) {
      // tst(TOK_NUM); // TODO
    } else if (op == OP_SR) {
      // tst(TOK_SR); // TODO
    } else if (op == OP_R) {
      // return
      // pop the two labels from the stack
      sp++;
      sp++;
      // pop the next into the program counter.
      pc= m[sp++];
    } else if (op == OP_SET) {
      match= 1;
    } else if (op == OP_BE) {
      /* "branch to error" */
      assert(match);
    } else if (op == OP_CI) {
      // printf("%s", lookup_by_index(lexval)); // TODO
      written= 1;
    } else if (op == OP_GN1) {
      if (m[sp-1] == -1) {
	m[sp-1]= gensym++;
      }
      printf("L%d\n", m[sp-1]);
      indent= 1;
      written= 0;
    } else if (op == OP_GN2) {
      if (m[sp-2] == -1) {
	m[sp-2]= gensym++;
      }
      printf("L%d\n", m[sp-2]);
      indent= 1;
      written= 0;
    } else if (op == OP_LB) {
      indent= 0;
    } else if (op == OP_OUT) {
      if (written) {
	printf("\n");
	indent= 1;
	written= 0;
      }
    } else {
      // ops with arguments
      arg= op % 16;
      op= op / 16;
      if (op == OP_TST) {
	// TODO: handle an upcoming string,
	// the length of which is stored in the arg
      } else if (op == OP_CLL) {
	// call
	m[sp - 1]= pc;
	m[sp - 2]= -1;
	m[sp - 3]= -1;
	pc= arg;
      } else if (op == OP_B) {
	pc= arg;
      } else if (op == OP_BT) {
	if (match) {
	  pc= arg;
	}
      } else if (op == OP_BF) {
	if (! match) {
	  pc= arg;
	}
      } else if (op == OP_CL) {
	while (arg--) {
	  putchar(m[pc++]);
	}
      } else if (op == OP_ADR) {
	// call
	m[sp - 1]= -1; // return to -1?
	m[sp - 2]= -1;
	m[sp - 3]= -1;
	pc= arg;
      } else {
	; // unknown op
      }
    }
  }
  return 0;
}
