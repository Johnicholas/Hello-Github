A little embedded-lua / curses roguelike thing
----------------------------------------------

To run this, you need a linux machine (probably) with lua5.0 and curses installed.

To install lua5.0, something like this::

  sudo apt-get install liblua50-dev liblualib50-dev

would probably work, depending on your distribution.

Then 'make lua_curses' should build a binary called 'lua_curses'.

Then './lua_curses game_loop.lua' ought to show a little curses thing (control-C to quit).

The setup / draw / update structure was taken from ippa's JawsJS.
The game loop was based on Steven Fuerst's 512-byte roguelike.

