#include <assert.h>
#include <stdio.h>
#include "vtprolog.h"

void strip_trailing_blanks(char* s) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

void val(char* s, float num, int code) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

void getmem(void* p, int bytes) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

void* seg(void* p) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

void* ofs(void* p) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}  

int memavail() {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

int pos(char* search_pattern, char* text_to_search) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

void delete(char* to_modify, int from, int to) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

int in(char to_test, char* char_set) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

char* copy(char* to_copy, int from, int to) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

char* concat(char* first, char* second) {
  // TODO(johnicholas.hines@gmail.com): Stop calling this function.
}

void test_vtprolog_open() {
  // happy path
  system("touch dummy.txt"); // TODO(johnicholas.hines@gmail.com): Some tmpfile thing?
  FILE* to_open= NULL;
  assert(vtprolog_open(&to_open, "dummy.txt"));
  assert(to_open != NULL);
  system("rm -f dummy.txt");
}

// TODO(johnicholas.hines@gmail.com): This isn't true in general, but it's true for my purposes right now.
void test_stdin_is_a_console() {
  assert(is_console(stdin));
}

void test_dummy_file_is_not_a_console() {
  system("touch dummy.txt");
  FILE* dummy_file= fopen("dummy.txt", "r");
  assert(!is_console(dummy_file));
  fclose(dummy_file);
  system("rm -f dummy.txt");
}  

int main() {
  test_vtprolog_open();
  test_stdin_is_a_console();
  test_dummy_file_is_not_a_console();
  printf("Hello world!\n");
  return 0;
}

