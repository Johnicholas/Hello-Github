// Based on Steven Fuerst's obfuscated, 512-byte roguelike "Monster Cave".
MonsterCave = {
    rows : 25,
    cols : 80,
    rooms : 50,
    monsters : 10,
    map : [],
    loc : [],
    hp : [],

    setup : function () {
	var row = 0;
	var col = 0;

	// for each cell in the map
	for (row = 0; row < this.rows; ++row) {
	    this.map[row] = []
	    for (col = 0; col < this.cols; ++col) {
		// put a wall
		this.map[row][col] = '#'
	    }
	}

	// make a messy cave by opening out a lot of rectangles
	var i
	for (i = 0; i < this.rooms; ++i) {
	    var height = rand(3, 6)
	    var width = rand(1, 25)
	    var top = rand(1, this.rows - height - 1)
	    var left = rand(1, this.cols - width - 1)
	    var bottom = top + height
	    var right = left + width
	    for (row = top; row <= bottom; ++row) {
		for (col = left; col <= right; ++col) {
		    this.map[row][col] = '.'
		}
	    }
	}
	// place entities (player and monsters)
	var i
	for (i = 0; i < this.monsters + 1; ++i) {
	    while (true) {
		var row = rand(1, this.rows - 1)
		var col = rand(1, this.cols - 1)
		if (this.map[row][col] == '.') {
		    this.loc[i] = { row : row, col : col }
		    if (i == 0) {
			// the player
			this.map[row][col] = '@'
			this.hp[i] = 5
		    } else {
			this.map[row][col] = 'M'
			this.hp[i] = 2
		    }
		    break
		}
	    }
	}
	// place the stairway
	while (true) {
	    var row = rand(1, this.rows - 1)
	    var col = rand(1, this.cols - 1)
	    if (this.map[row][col] == '.') {
		this.map[row][col] = '>'
		break
	    }
	}
    },

    draw : function () {
	var row = 0;
	var col = 0;
	for (row = 0; row < this.rows; ++row) {
	    move(row, 0);
	    for (col = 0; col < this.cols; ++col) {
		addch(this.map[row][col])
	    }
	}
	// put the cursor on the player
	move(this.loc[0].row, this.loc[0].col)
    },

    update : function (key) {
	if (key) {
	    // move entities (player and monsters)
	    var i
	    for (i = 0; i < this.monsters + 1; ++i) {
		var oldrow = this.loc[i].row
		var newrow = oldrow
		var oldcol = this.loc[i].col
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
		    if (this.map[newrow][newcol] == '>') {
			// going down is just like starting over! :P
			this.setup()
			return
		    }
		    var j
		    for (j = 1; j < this.monsters; ++j) {
			if (this.loc[j].row == newrow && this.loc[j].col == newcol) {
			    // player stepped on a monster
			    if (this.hp[j] > 0) {
				// it was alive at the time
				this.hp[j] = this.hp[j] - 1
				if (this.hp[j] == 0) {
				    // now it's dead...
				    this.map[this.loc[j].row][this.loc[j].col] = 'D'
				}
			    }
			}
		    }
		} else {
		    if (this.hp[i] > 0) {
			// move a monster
			newrow += rand(0, 3) - 1
			newcol += rand(0, 3) - 1
			if (newrow == this.loc[0].row && newcol == this.loc[0].col) {
			    // monster stepped on the player
			    this.hp[0] = this.hp[0] - 1
			    if (this.hp[0] == 0) {
				// monster killed the player
				quit()
			    }
			}
		    }
		}
		// regardless, if an entity moved, the map needs to be updated
		if (this.map[newrow][newcol] == '.') {
		    this.loc[i] = { row : newrow, col : newcol }
		    this.map[newrow][newcol] = this.map[oldrow][oldcol]
		    this.map[oldrow][oldcol] = '.'
		}
	    }
	}
    }
}

start(MonsterCave)
