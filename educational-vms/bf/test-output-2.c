#include <stdio.h>
#include <stdlib.h>

int a[32ul];

int main(int argc, char **argv)
{
  size_t p = 0ul;
  int temp;
  int c;
  a[p] += 3;
  a[(p + 1ul) % 32ul] += 4 * a[p];
  if ((a[(p + 1ul) % 32ul] % 2) != 0)
       return 2;
  a[(p + 1ul) % 32ul] /= 2;
  a[(p + 1ul) % 32ul] = 715827883 * a[(p + 1ul) % 32ul];
  a[(p + 1ul) % 32ul] &= 2147483647;
  a[(p + 2ul) % 32ul] += a[(p + 1ul) % 32ul];
  printf("%d\n", a[(p + 2ul) % 32ul]);
  a[(p + 3ul) % 32ul] += 4 * a[(p + 2ul) % 32ul];
  p = (p + 3ul) % 32ul;
  a[(p + 29ul) % 32ul] = 0;
  a[(p + 30ul) % 32ul] = 0;
  a[(p + 31ul) % 32ul] = 0;
  a[(p + 1ul) % 32ul] += 4;
  while (a[p] != 0) {
  a[(p + 2ul) % 32ul] += 3 * a[(p + 1ul) % 32ul];
  a[(p + 1ul) % 32ul] = a[(p + 2ul) % 32ul];
  printf("%d\n", a[(p + 1ul) % 32ul]);
  a[p] += -1;
  a[(p + 2ul) % 32ul] = 0;
  }
  a[(p + 1ul) % 32ul] = 858993459 * a[(p + 1ul) % 32ul];
  a[(p + 2ul) % 32ul] += -2 * a[(p + 1ul) % 32ul];
  printf("%d\n", a[(p + 2ul) % 32ul]);
  a[(p + 3ul) % 32ul] += 4;
  a[(p + 4ul) % 32ul] += 4 * a[(p + 3ul) % 32ul];
  a[(p + 1ul) % 32ul] = 0;
  a[(p + 3ul) % 32ul] = 0;
  a[(p + 4ul) % 32ul] += 3;
  a[(p + 5ul) % 32ul] += 1;
  while (a[(p + 4ul) % 32ul] != 0) {
  a[(p + 6ul) % 32ul] += 3 * a[(p + 5ul) % 32ul];
  a[(p + 4ul) % 32ul] += -1;
  a[(p + 5ul) % 32ul] = a[(p + 6ul) % 32ul];
  a[(p + 6ul) % 32ul] = 0;
  }
  a[(p + 5ul) % 32ul] = 1431655765 * a[(p + 5ul) % 32ul];
  a[(p + 2ul) % 32ul] += -3 * a[(p + 5ul) % 32ul];
  printf("%d\n", a[(p + 2ul) % 32ul]);
  p = (p + 2ul) % 32ul;
  a[(p + 3ul) % 32ul] = 0;
  if (a[p] != 0)
       return 2;
  if ((c = getchar()) == EOF)
     a[p] = -1;
  else
     a[p] = c;
  return 0;
}
