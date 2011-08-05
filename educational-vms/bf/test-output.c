#include <stdio.h>
#include <stdlib.h>

unsigned char a[32ul];

int main(int argc, char **argv)
{
  size_t p = 0ul;
  unsigned char temp;
  int c;
  a[p] += 2u;
  a[(p + 1ul) % 32ul] += 3u * a[p];
  if ((c = getchar()) == EOF)
     a[(p + 2ul) % 32ul] = 0u;
  else
     a[(p + 2ul) % 32ul] = c;
  a[(p + 2ul) % 32ul] += a[(p + 1ul) % 32ul];
  putchar((unsigned char)a[(p + 2ul) % 32ul]);
  a[(p + 3ul) % 32ul] += 4u;
  if ((a[(p + 3ul) % 32ul] % 2u) != 0)
       return 2;
  a[(p + 3ul) % 32ul] /= 2u;
  a[(p + 3ul) % 32ul] = 127u * a[(p + 3ul) % 32ul];
  a[(p + 3ul) % 32ul] &= 127u;
  a[(p + 4ul) % 32ul] += a[(p + 3ul) % 32ul];
  putchar((unsigned char)a[(p + 4ul) % 32ul]);
  p = (p + 4ul) % 32ul;
  a[(p + 28ul) % 32ul] = 0u;
  a[(p + 29ul) % 32ul] = 0u;
  a[(p + 31ul) % 32ul] = 0u;
  return 0;
}
