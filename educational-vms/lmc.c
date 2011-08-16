// An implementation of Stuart Madnick's "Little Man Computer".
// There seem to be a few slightly different instruction sets for the lmc
// floating around out there - I just picked one.
#include <assert.h>
#include <stdio.h>

int m[100];

void run() {
  int pc= 0;
  int a= 0;

  while (1) {
    int op= m[pc] / 100;
    int xx= m[pc] % 100;
    pc++; /* by default, step to the next instruction */
    switch (op) {
    case 1: /* load 1xx loads contents of mailbox xx into calculator */
      a= m[xx];
      break;
    case 2: /* store 2xx stores the calculator into mailbox xx */
      m[xx]= a;
      break;
    case 3: /* add 3xx adds the contents of mailbox xx into calculator */
      a += m[xx];
      break;
    case 4: /* sub 4xx subtracs the value in mailbox xx from the calculator */
      a -= m[xx];
      break;
    case 5: /* branch 5xx change the program counter to xx */
      pc= xx;
      break;
    case 6: /* 6xx branch if the calculator value is nonnegative (to xx) */
      if (a >= 0) {
	pc= xx;
      }
      break;
    case 7: /* 7xx branch if the calculator value is zero (to xx) */
      if (a == 0) {
	pc= xx;
      }
    case 0:
      switch (xx) {
      case 1: /* input 001 accept a number from the input */
	assert(scanf("%d", &a) == 1);
	break;
      case 2: /* output 002 display the number in the calculator */
	printf("%d\n", a);
	break;
      case 3: /* halt 003 stop the program */
	return;
	break;
      default:
	assert(0);
	break;
      }
      break;
    default:
      assert(0);
      break;
    }
  }
}
    
void load(FILE* to_load) {
  int i;
  for (i= 0; i < 100; ++i) {
    if (fscanf(to_load, " %d", m+i) != 1) {
      break;
    }
  }
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  FILE* program= fopen(argv[1], "r");
  assert(program);
  load(program);
  run();
  return 0;
}

