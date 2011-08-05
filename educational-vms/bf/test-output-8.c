#include <stdio.h>
#include <stdlib.h>

unsigned a[32ul];

int main(int argc, char **argv)
{
  size_t p = 0ul;
  unsigned temp;
  if (1 != scanf("%u", &a[p]))
     a[p] = 4294967295u;
  a[p] += 1u;
  while (a[p] != 0) {
  a[p] += 4294967295u;
  printf("%u\n", a[p]);
  if (1 != scanf("%u", &a[p]))
     a[p] = 4294967295u;
  a[p] += 1u;
  }
  return 0;
}
