Extending and embedding python with curses
------------------------------------------

This is a demo / learning project / reminder of how to extend-and-embed python.
To build it, you need libpython of some version - currently it wants 2.7,
but it shouldn't matter much. Do something like::

  sudo apt-get install libpython2.7-dev

Or if you want to try a different version, edit the Makefile.
Then to build py_curses do::

  make

Then to run the example game loop (based on Fuerst's 512-byte roguelike Monster Cave),
do::

  ./py_curses game_loop.py

