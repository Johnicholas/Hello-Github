extern "C" {
  #include "io.h"
}

#include <cassert>

int main(int argc, char* argv[]) {
  assert(get_char() == 'h');
  assert(get_char() == 'i');
  put_back('o');
  put_back('l');
  put_back('l');
  put_back('e');
  put_back('h');
  assert(get_char() == 'h');
  assert(get_char() == 'e');
  assert(get_char() == 'l');
  assert(get_char() == 'l');
  assert(get_char() == 'o');
  assert(get_char() == '!');
  return 0;
}

