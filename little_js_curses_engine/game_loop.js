function Game() {
    var row = 0
    var col = 0
    var turns = 0

    this.setup = function () {
	row = 4
	col = 5
    }

    this.draw = function () {
	// Draw the floor
	for (i = 0; i < 24; i++) {
	    move(i, 0)
	    for (j = 0; j < 80; j = j + 1) {
		addch('.')
	    }
	}
	// Draw the player
	move(row, col)
	addch('@')
	// Move the cursor on back on top of the player
	move(row, col)
    }

    this.update = function (keypressed) {
	if (keypressed) {
	    if (keypressed == 'w') {
		row = row - 1
	    } else if (keypressed == 'a') {
		col = col - 1
	    } else if (keypressed == 's') {
		row = row + 1
	    } else if (keypressed == 'd') {
		col = col + 1
	    }
	} else {
	    // Do nothing
	}
	turns = turns + 1
	if (turns > 100) {
	    quit()
	}
    }
}

start(Game)
