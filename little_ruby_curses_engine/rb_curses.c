#include <assert.h>
#include <string.h>
#include <curses.h>
#include <math.h>
#include "ruby.h"
#include <unistd.h>

static VALUE my_addch(VALUE self, VALUE to_add_val) {
  const char* to_add_cstr = StringValueCStr(to_add_val);
  assert(strlen(to_add_cstr) == 1);
  addch(to_add_cstr[0]);
  return Qtrue;
}


static VALUE my_move(VALUE self, VALUE row_val, VALUE col_val) {
  move(NUM2INT(row_val), NUM2INT(col_val));
  return Qtrue;
}


static VALUE my_rand(VALUE self, VALUE min_val, VALUE max_val) {
  int min = NUM2INT(min_val);
  int max = NUM2INT(max_val);
  return INT2NUM(rand() % (max - min) + min);
}

static VALUE my_quit(VALUE self) {
  endwin();
  exit(0);
  return Qtrue;
}

static VALUE my_start(VALUE self, VALUE to_start) {
  ID setup_method = rb_intern("setup");
  ID draw_method = rb_intern("draw");
  ID update_method = rb_intern("update");
  assert(rb_respond_to(to_start, setup_method));
  assert(rb_respond_to(to_start, draw_method));
  assert(rb_respond_to(to_start, update_method));
  rb_funcall(to_start, setup_method, 0);
  while (1) {
    rb_funcall(to_start, draw_method, 0);
    refresh();
    rb_funcall(to_start, update_method, 1, INT2NUM(getch()));
  }
  return Qtrue;
}

static VALUE my_module;

// Note: Init_Classname is a rigid name scheme?
void Init_LittleCurses() {
  my_module = rb_define_module("LittleCurses");
  rb_define_method(my_module, "addch", my_addch, 1);
  rb_define_method(my_module, "move", my_move, 2);
  rb_define_method(my_module, "rand", my_rand, 2);
  rb_define_method(my_module, "quit", my_quit, 0);
  rb_define_method(my_module, "start", my_start, 1);
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  ruby_init();
  ruby_init_loadpath();
  Init_LittleCurses();
  ruby_script("embedded");
  // initialize curses
  initscr(); 
  // don't buffer user input into lines, 
  // but Ctrl-Z and Ctrl-C work as usual
  cbreak();
  // don't echo user input.
  noecho();
  // getch returns ERR if the user takes more than a
  // tenth of a second
  // halfdelay(1); // TODO
  // don't ignore function keys F1, F2, etc.
  keypad(stdscr, TRUE);
  // TODO: seed the random number generator with the time.
  int status;
  rb_load_protect(rb_str_new2(argv[1]), 0, &status);
  // status = ruby_exec();

  endwin();
  return 0;
}

