/* This program is in the public domain. */

/* Reference BF interpreter. */

/* Array size: 1048576 wraparound; cell size: 0 to 255 wraparound. */
/* EOF processing: value 0. */
/* Maximum loop nesting: 1000. */

#include <stdio.h>
#include <stdlib.h>

#define SIZE (1ul << 22)
#define TAPE_SIZE (1ul << 20)
#define STACK_SIZE 1000

unsigned char bf_buffer[SIZE];
size_t bf_loops[SIZE];
unsigned char tape[TAPE_SIZE];

int main(int argc, char **argv)
{
  FILE *f;
  size_t len, pc = 0, sp = 0, position = 0;
  size_t stack[STACK_SIZE];
  int c;

  if (argc != 2)
  {
    fprintf(stderr, "Usage: %s <BF program file name>\n", argv[0]);
    return 99;
  }

  f = fopen(argv[1], "r");
  if (f == NULL)
  {
    fprintf(stderr, "Cannot open input file.\n");
    return 98;
  }
  if ((len = fread(bf_buffer, 1, SIZE, f)) == SIZE)
  {
    fprintf(stderr, "Program too large, max is %lu characters.\n", SIZE);
    return 97;
  }
  (void)fclose(f);
  for (pc = 0; pc != len; pc++)
  {
    if (bf_buffer[pc] == '[')
    {
      if (sp == STACK_SIZE)
      {
        fprintf(stderr, "Loop nesting too deep, max nest is %d.\n",
                                   STACK_SIZE);
        return 96;
      }
      stack[sp++] = pc;
    }
    else if (bf_buffer[pc] == ']')
    {
      if (sp == 0)
      {
        fprintf(stderr, "Ill-formed program.\n");
        return 95;
      }
      bf_loops[stack[--sp]] = pc;
      bf_loops[pc] = stack[sp];
    }
  }
  if (sp != 0)
  {
    fprintf(stderr, "Ill-formed program.\n");
    return 95;
  }
    
  for (pc = 0; pc != len; pc++)
  {
    switch (bf_buffer[pc])
    {
      case '+': tape[position]++; break;
      case '-': tape[position]--; break;
      case '>': position = (position + 1) & (TAPE_SIZE - 1); break;
      case '<': position = (position - 1) & (TAPE_SIZE - 1); break;
      case '.': putchar(tape[position]); break;
      case ',': if ((c = getchar()) == EOF)
                   tape[position] = 0;
                else
                   tape[position] = c;
                break;
      case '[': if (tape[position] == 0)
                   pc = bf_loops[pc];
                break;
      case ']': if (tape[position] != 0)
                   pc = bf_loops[pc];
                break;
      default:  break;
    }
  }
  return 0;
}
