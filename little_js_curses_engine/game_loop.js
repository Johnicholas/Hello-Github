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
		    loc[i] = [row, col]
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
	for (row = 0; row < 25; ++row) {
	    move(row, 0);
	    for (col = 0; col < 80; ++col) {
		addch(map[row][col])
	    }
	}
    }

    this.update = function (key) {
	// move entities (player and monsters)
	var i
	for (i = 0; i < monsters + 1; ++i) {
	    var oldrow = loc[i][0]
	    var newrow = loc[i][0]
	    var oldcol = loc[i][1]
	    var newcol = loc[i][1]
	    if (i == 0) {
		// move the player
		if (key) {
		    if (key == 'w') {
			--newrow;
		    } else if (key == 'a') {
			--newcol;
		    } else if (key == 's') {
			++newrow;
		    } else if (key == 'd') {
			++newcol;
		    }
		}
		if (map[newrow][newcol] == '>') {
		    this.setup()
		}
		// TODO: check whether player stepped on a monster
		// TODO: check whether player killed a monster
	    } else {
		// move a monster
		newrow += rand(0, 3) - 1
		newcol += rand(0, 3) - 1
		// TODO: check whether monster stepped on a player
		// TODO: check whether monster killed a player
	    }
	    // regardless, if an entity moved, the map needs to be updated
	    if (map[newrow][newcol] == '.') {
		loc[i] = [newrow, newcol]
		map[newrow][newcol] = map[oldrow][oldcol]
		map[oldrow][oldcol] = '.'
	    }
	}
    }
}

start(Game)
