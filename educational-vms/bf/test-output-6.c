#include <stdio.h>
#include <stdlib.h>

int a[32ul];

int main(int argc, char **argv)
{
  size_t p = 0ul;
  int temp;
  if (1 != scanf("%d", &a[p]))
     a[p] = -1;
  a[p] += 1;
  while (a[p] != 0) {
  a[p] += -1;
  printf("%d\n", a[p]);
  if (1 != scanf("%d", &a[p]))
     a[p] = -1;
  a[p] += 1;
  }
  return 0;
}
