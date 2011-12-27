#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <curses.h>
extern "C" {
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}
#include "math.h"
#include <unistd.h>

int my_addch(lua_State* lua) {
  assert(lua_gettop(lua) == 1); // one argument
  assert(lua_isstring(lua, 1)); // a string
  const char* to_add = lua_tostring(lua, 1); // unpack it.
  assert(strlen(to_add) == 1);
  addch(to_add[0]);
  return 0; // zero return values
}

int my_getch(lua_State* lua) {
  assert(lua_gettop(lua) == 0); // zero arguments
  lua_pushnumber(lua, getch());
  return 1; // one return value
}

int my_move(lua_State* lua) {
  assert(lua_gettop(lua) == 2); // two arguments
  assert(lua_isnumber(lua, 1)); 
  int row= round(lua_tonumber(lua, 1));
  assert(lua_isnumber(lua, 2)); 
  int col= round(lua_tonumber(lua, 2));
  move(row, col);
  return 0; // zero return values
}

int my_rand(lua_State* lua) {
  assert(lua_gettop(lua) == 2); // 2 arguments
  assert(lua_isnumber(lua, 1));
  int min = round(lua_tonumber(lua, 1));
  assert(lua_isnumber(lua, 2));
  int max = round(lua_tonumber(lua, 2));
  lua_pushnumber(lua, rand() % (max - min) + min);
  return 1; // one return value
}

int my_quit(lua_State* lua) {
  endwin();
  exit(0);
  return 0; // not actually executed
}

int my_start(lua_State* lua) {
  assert(lua_gettop(lua) == 1); // 1 argument
  assert(lua_istable(lua, 1)); // it should be a table
  // setup
  lua_pushstring(lua, "setup"); // stack looks like [gametable, "setup"]
  lua_gettable(lua, -2); // stack looks like [gametable, setupfunction]
  assert(lua_isfunction(lua, -1));
  lua_pushvalue(lua, -2); // stack is [gametable, setupfunction, gametable]
  lua_pcall(lua, 1, 0, 0); // stack is now [gametable]
  while (1) {
    // draw
    lua_pushstring(lua, "draw");
    lua_gettable(lua, -2); 
    // stack is [gametable, drawfunction]
    assert(lua_isfunction(lua, -1));
    lua_pushvalue(lua, -2);
    // stack is [gametable, drawfunction, gametable]
    lua_pcall(lua, 1, 0, 0);

    refresh();

    // update
    int c = getch();
    lua_pushstring(lua, "update");
    lua_gettable(lua, -2);
    assert(lua_isfunction(lua, -1));
    lua_pushvalue(lua, -2); // stack is [gametable, drawfunction, gametable]
    lua_pushnumber(lua, c); // stack is [gametable, drawfunction, gametable, c]
    lua_pcall(lua, 2, 0, 0);
  }

  return 0;
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    fprintf(stderr, "lua_curses something.lua\n");
    exit(1);
  }
  
  lua_State* lua= lua_open();
  luaopen_base(lua);
  lua_settop(lua, 0);
  luaopen_string(lua);
  lua_settop(lua, 0);
  // bindings
  lua_pushcfunction(lua, my_addch);
  lua_setglobal(lua, "addch");
  lua_pushcfunction(lua, my_getch);
  lua_setglobal(lua, "getch");
  lua_pushnumber(lua, ERR);
  lua_setglobal(lua, "ERR");
  lua_pushcfunction(lua, my_move);
  lua_setglobal(lua, "move");
  lua_pushcfunction(lua, my_rand);
  lua_setglobal(lua, "rand");
  lua_pushcfunction(lua, my_quit);
  lua_setglobal(lua, "quit");
  lua_pushcfunction(lua, my_start);
  lua_setglobal(lua, "start");

  initscr(); // initialize curses
  cbreak(); // don't buffer user input into lines, but Ctrl-Z and Ctrl-C work as usual
  noecho(); // don't echo user input.
  halfdelay(1); // getch returns ERR if the user takes more than a tenth of a second
  keypad(stdscr, TRUE); // don't ignore function keys F1, F2, etc.
  // TODO: seed the random number generator with the time.

  if (lua_dofile(lua, argv[1])) {
    fprintf(stderr, "lua_curses had a problem opening %s\n", argv[1]);
    endwin();
    exit(1);
  }
  fprintf(stderr, "hello world\n");

  lua_close(lua);
  endwin();
  return 0;
}
