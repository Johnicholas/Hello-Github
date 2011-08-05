#include <stdio.h>
#include <stdlib.h>

int a[32ul];

int main(int argc, char **argv)
{
  size_t p = 0ul;
  int temp;
  int c;
  if ((c = getchar()) == EOF)
     a[p] = -1;
  else
     a[p] = c;
  a[p] += 1;
  while (a[p] != 0) {
  a[p] += -1;
  putchar((unsigned char)(unsigned)a[p]);
  if ((c = getchar()) == EOF)
     a[p] = -1;
  else
     a[p] = c;
  a[p] += 1;
  }
  return 0;
}
