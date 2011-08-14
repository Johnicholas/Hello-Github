#include "io.h"

/* consider pulling these configuration things out into another header */
#define BUFFER_SIZE 500
static char buffer[BUFFER_SIZE];
static int buffer_pointer;

void put_back(char c) {
  buffer[buffer_pointer++]= c;
}

int get_char() {
  if (buffer_pointer > 0) {
    return buffer[--buffer_pointer];
  } else {
    return getchar();
  }
}

