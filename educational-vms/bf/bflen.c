/* This program is in the public domain. */
/* Compute the length of a BF program read from stdin. */
/* Comments are not counted. */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
  int c, len = 0;

  while ((c = getchar()) != EOF)
  {
    switch (c)
    {
      case '+': case '-':
      case '>': case '<':
      case '[': case ']':
      case ',': case '.':
                len++;
                break;

      default:
                break;
    }
  }
  printf("%d characters\n", len);
  return 0;
}
