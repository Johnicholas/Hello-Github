#include <Python.h>
#include <cassert>
#include <cstdio>
#include <curses.h>


static PyObject* my_addch(PyObject* self, PyObject* args) {
  char to_add;
  PyArg_ParseTuple(args, "c", &to_add);
  addch(to_add);
  return Py_BuildValue("");
}

static PyMethodDef my_curses_methods[] = {
  { "addch", my_addch, METH_VARARGS, "add a character to the screen" },
  { 0, 0, 0, 0 }
};

PyMODINIT_FUNC init_extensions() {
  Py_InitModule("little_curses", my_curses_methods);
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  Py_SetProgramName(argv[0]);
  Py_Initialize();
  init_extensions();

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
  PyRun_SimpleFile(fopen(argv[1], "r"), argv[1]);

  // TODO: these two are just a hack to see something work.
  refresh();
  sleep(1);

  endwin();
  Py_Finalize();
  return 0;
}

