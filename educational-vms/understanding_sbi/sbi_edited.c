/* simple brainfuck interpreter */
/* 20 April 2006 */
/* Daniel B. Cristofani */
/* http://www.brainfuck.org/ */
/* edited by johnicholas while reading it */

#include <stdio.h>
#include <stdlib.h>

#define ARRAY_SIZE 65536
#define MAX_CODE_SIZE 65536

// For simplicity, we'll use statically allocated arrays with matching indices.
char code[MAX_CODE_SIZE]; // copy of the program we'll read into memory.
int codelength; 
int targets[MAX_CODE_SIZE]; // to save matching '[' for each ']' and vice versa.

void check(int test, const char* complaint) {
  if (test) {
    fprintf(stderr, "%s\n", complaint);
    exit(1);
  }
}

void read_file(char* filename) {
  FILE* prog= fopen(filename, "r");
  check(prog == NULL, "Can't open the file.");
  codelength= fread(code, 1, MAX_CODE_SIZE, prog);
  fclose(prog);
}

void parse_code() {
  int stack[MAX_CODE_SIZE]; // to store locations of still-unmatched '['s.
  int stackp= 0;
  int codep;

  for (codep= 0; codep < codelength; codep++) {
    if (code[codep] == '[') {
      // put each '[' on the stack
      stack[stackp++]= codep;
    }
    if (code[codep] == ']') {
      // If we meet a ']',
      // and there is no '[' left on the stack, it's an error.
      check(stackp == 0, "Unmatched ']'.");

      // if there is one, we take the matching '[' from the stack top,
      --stackp;
      // save it as the match for the current ']',
      targets[codep]= stack[stackp];
      // and save the current ']' as the match for it.
      targets[stack[stackp]]= codep;
    }
  }
  check(stackp > 0, "Unmatched '['.");
}

void run() {
  // the memory used by the brainfuck program.
  short int mem[ARRAY_SIZE]; 
  short int memp= 0;
  int c;
  int codep;

  for (codep= 0; codep < codelength; codep++) {
    switch (code[codep]) {
    case '+':
      mem[memp]++;
      break;
    case '-':
      mem[memp]--;
      break;
    case '<': 
      memp--;
      break;
    case '>':
      memp++;
      break;
    case ',':
      c= getchar();
      if (c != EOF) {
	mem[memp]= (c == '\n' ? 10 : c);
      }
      break;
    case '.':
      putchar(mem[memp] == 10 ? '\n' : mem[memp]);
      break;
    case '[':
      if (!mem[memp]) {
	codep= targets[codep];
      }
      break;
    case ']':
      if (mem[memp]) {
	codep= targets[codep];
      }
      break;
    }
  }
}

int main(int argc, char *argv[]) {
  check(argc > 2, "Too many arguments.");
  check(argc < 2, "I need a program filename.");
  read_file(argv[1]);
  parse_code();
  // Everything is okay; we start executing the program.
  run();
  return 0;
}
