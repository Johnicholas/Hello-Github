// Based on Steven Fuerst's obfuscated, 512-byte roguelike "Monster Cave".
function Game() {
    // parameters
    var rows = 25
    var cols = 80
    var rooms = 50
    var monsters = 10

    var map = []
    var loc = []
    var hp = []
    var turns = 0

    this.setup = function () {
	var row = 0;
	var col = 0;
	// each cell in the map
	for (row = 0; row < rows; ++row) {
	    map[row] = []
	    for (col = 0; col < cols; ++col) {
		map[row][col] = '#' // a wall
	    }
	}

	// make a messy cave by opening out a lot of rectangles
	var i
	for (i = 0; i < rooms; ++i) {
	    var height = rand(3, 6)
	    var width = rand(1, 25)
	    var top = rand(1, rows - height - 1)
	    var left = rand(1, cols - width - 1)
	    var bottom = top + height
	    var right = left + width
	    for (row = top; row <= bottom; ++row) {
		for (col = left; col <= right; ++col) {
		    map[row][col] = '.'
		}
	    }
	}
	// place entities (player and monsters)
	var i
	for (i = 0; i < monsters + 1; ++i) {
	    while (true) {
		var row = rand(1, rows - 1)
		var col = rand(1, cols - 1)
		if (map[row][col] == '.') {
		    loc[i] = { row : row, col : col }
		    if (i == 0) {
			// the player
			map[row][col] = '@'
			hp[i] = 5
		    } else {
			map[row][col] = 'M'
			hp[i] = 2
		    }
		    break
		}
	    }
	}
	// place the stairway
	while (true) {
	    var row = rand(1, rows - 1)
	    var col = rand(1, cols - 1)
	    if (map[row][col] == '.') {
		map[row][col] = '>'
		break
	    }
	}
    }

    this.draw = function () {
	var row = 0;
	var col = 0;
	for (row = 0; row < rows; ++row) {
	    move(row, 0);
	    for (col = 0; col < cols; ++col) {
		addch(map[row][col])
	    }
	}
    }

    this.update = function (key) {
	if (key) {
	    // move entities (player and monsters)
	    var i
	    for (i = 0; i < monsters + 1; ++i) {
		var oldrow = loc[i].row
		var newrow = oldrow
		var oldcol = loc[i].col
		var newcol = oldcol
		if (i == 0) {
		    // move the player
		    if (key == 'w') {
			--newrow;
		    } else if (key == 'a') {
			--newcol;
		    } else if (key == 's') {
			++newrow;
		    } else if (key == 'd') {
			++newcol;
		    }
		    if (map[newrow][newcol] == '>') {
			// going down is just like starting over! :P
			this.setup()
			return
		    }
		    var j
		    for (j = 1; j < monsters; ++j) {
			if (loc[j].row == newrow && loc[j].col == newcol) {
			    // player stepped on a monster
			    if (hp[j] > 0) {
				// it was alive at the time
				hp[j] = hp[j] - 1
				if (hp[j] == 0) {
				    // now it's dead...
				    map[loc[j].row][loc[j].col] = 'D'
				}
			    }
			}
		    }
		} else {
		    if (hp[i] > 0) {
			// move a monster
			newrow += rand(0, 3) - 1
			newcol += rand(0, 3) - 1
			if (newrow == loc[0].row && newcol == loc[0].col) {
			    // monster stepped on the player
			    hp[0] = hp[0] - 1
			    if (hp[0] == 0) {
				// monster killed the player
				quit()
			    }
			}
		    }
		}
		// regardless, if an entity moved, the map needs to be updated
		if (map[newrow][newcol] == '.') {
		    loc[i] = { row : newrow, col : newcol }
		    map[newrow][newcol] = map[oldrow][oldcol]
		    map[oldrow][oldcol] = '.'
		}
	    }
	}
    }
}

start(Game)
