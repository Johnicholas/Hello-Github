#include <assert.h>
#include <curses.h>
#include "ruby.h"
#include <unistd.h>

static VALUE my_foo(VALUE self) {
  addch('h');
  addch('i');
  refresh();
  sleep(1);
  return self;
}

/*
static VALUE my_addch(VALUE self, VALUE to_add) {

}

static VALUE my_start(VALUE self, VALUE to_start) {
// rb_funcall(receiver, method_id, argc, ...);
// rb_eval_string(to_eval);
}

*/

static VALUE my_module;

// Note: Init_Classname is a rigid name scheme?
void Init_LittleCurses() {
  my_module = rb_define_module("LittleCurses");
  rb_define_method(my_module, "foo", my_foo, 0);
  // rb_define_method(my_module, "addch", my_addch, 1);
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  ruby_init();
  ruby_init_loadpath();
  Init_LittleCurses();
  ruby_script("embedded");
  int status;
  rb_load_protect(rb_str_new2(argv[1]), 0, &status);
  if (status == 0) {
    // initialize curses
    initscr(); 
    // don't buffer user input into lines, 
    // but Ctrl-Z and Ctrl-C work as usual
    cbreak();
    // don't echo user input.
    noecho();
    // getch returns ERR if the user takes more than a tenth of a second
    halfdelay(1);
    // don't ignore function keys F1, F2, etc.
    keypad(stdscr, TRUE);
    // TODO: seed the random number generator with the time.

    status = ruby_exec();

    endwin();
  }
  return 0;
}

