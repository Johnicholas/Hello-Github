#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

int eval(const char* opcodes) {
  int s[1000];
  int sp= 0;
  int temp;
  int pc;
  
  for (pc= 0; pc < strlen(opcodes); ++pc) {
    if (isdigit(opcodes[pc])) {
      s[sp++]= opcodes[pc] - '0';
    } else {
      assert(sp >= 2);
      temp= s[--sp];
      switch (opcodes[pc]) {
      case '+': s[sp-1] += temp; break;
      case '-': s[sp-1] -= temp; break;
      case '*': s[sp-1] *= temp; break;
      case '/': s[sp-1] /= temp; break;
      default: assert(0); break;
      }
    }
  }
  assert(sp == 1);
  return s[0];
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  printf("%d\n", eval(argv[1]));
  return 0;
}

