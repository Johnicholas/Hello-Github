#include <Python.h>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <curses.h>

static PyObject* my_addch(PyObject* self, PyObject* args) {
  char to_add;
  assert(PyArg_ParseTuple(args, "c", &to_add));
  addch(to_add);
  return Py_BuildValue("");
}

static PyObject* my_rand(PyObject* self, PyObject* args) {
  int min, max;
  assert(PyArg_ParseTuple(args, "ii", &min, &max));
  return Py_BuildValue("i", rand() % (max - min) + min);
}

static PyObject* my_move(PyObject* self, PyObject* args) {
  int row, col;
  assert(PyArg_ParseTuple(args, "ii", &row, &col));
  move(row, col);
  return Py_BuildValue("");
}

static PyObject* my_quit(PyObject* self, PyObject* args) {
  endwin();
  Py_Finalize();
  exit(0);
}

static PyObject* my_start(PyObject* self, PyObject* args) {
  PyObject* game;
  assert(PyArg_ParseTuple(args, "O", &game));
  // setup
  PyObject* result = PyObject_CallMethod(game, "setup", "");
  if (result == NULL) { return NULL; }
  Py_DECREF(result);
  while (1) {
    // draw
    result = PyObject_CallMethod(game, "draw", "");
    if (result == NULL) { return NULL; }
    Py_DECREF(result);
    refresh();
    // update
    int c = getch();
    result = PyObject_CallMethod(game, "update", "i", c);
    if (result == NULL) { return NULL; }
    Py_DECREF(result);
  }

  return Py_BuildValue("");
}

static PyObject* my_err(PyObject* self, PyObject* args) {
  return Py_BuildValue("i", ERR);
}

static PyMethodDef my_curses_methods[] = {
  { "addch", my_addch, METH_VARARGS, "add a character to the screen" },
  { "rand", my_rand, METH_VARARGS, "generate a random integer" },
  { "move", my_move, METH_VARARGS, "move the cursor" },
  { "quit", my_quit, METH_VARARGS, "quit the game" },
  { "start", my_start, METH_VARARGS, "start the game" },
  { "err", my_err, METH_VARARGS, "returns the number used by getch to indicate 'no key pressed'" },
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
  // halfdelay(1);
  // don't ignore function keys F1, F2, etc.
  keypad(stdscr, TRUE);
  // TODO: seed the random number generator with the time.

  PyRun_SimpleFile(fopen(argv[1], "r"), argv[1]);

  endwin();
  Py_Finalize();
  return 0;
}

