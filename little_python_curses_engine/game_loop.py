# based on Steven Fuerst's obfuscated, 512-byte roguelike "Monster Cave"

from little_curses import *

class Point:
    def __init__(self, row, col):
        self.row = row
        self.col = col

class MonsterCave:
    def __init__(self):
        self.rows = 25
        self.cols = 80
        self.rooms = 50
        self.monsters = 10
        self.map = {}
        self.loc = {}
        self.hp = {}

    def setup(self):
        for row in range(0, self.rows):
            self.map[row] = {}
            for col in range(0, self.cols):
                self.map[row][col] = '#'

        # make a messy cave by opening out a lot of rectangles
        for i in range(0, self.rooms):
            height = rand(3, 6)
            width = rand(1, 25)
            top = rand(1, self.rows - height - 1)
            left = rand(1, self.cols - width - 1)
            bottom = top + height
            right = left + width
            for row in range(top, bottom):
                for col in range(left, right):
                    self.map[row][col] = '.'

        # place entities (player and monsters)
        for i in range(0, self.monsters + 1):
            while True:
                row = rand(1, self.rows - 1)
                col = rand(1, self.cols - 1)
                if self.map[row][col] == '.':
                    self.loc[i] = Point(row, col)
                    if i == 0:
                        # the player
                        self.map[row][col] = '@'
                        self.hp[i] = 5
                    else:
                        # a monster
                        self.map[row][col] = 'M'
                        self.hp[i] = 2
                    break

        # place the stairway
        while True:
            row = rand(1, self.rows - 1)
            col = rand(1, self.cols - 1)
            if self.map[row][col] == '.':
                self.map[row][col] = '>'
                break

    def draw(self):
        assert type(self.map) == dict
        for row in range(0, self.rows):
            assert type(self.map[row]) == dict
            move(row, 0)
            for col in range(0, self.cols):
                assert type(self.map[row][col]) == str
                addch(self.map[row][col])

        # put the cursor on the player
        move(self.loc[0].row, self.loc[0].col)

    def update(self, key):
        if key == err():
            return
        else:
            # we've handled the special value,
            # convert it to a character
            key = chr(key)
        
        for i in range(0, self.monsters+1):
            oldrow = self.loc[i].row
            newrow = oldrow
            oldcol = self.loc[i].col
            newcol = oldcol
            if i == 0:
                
                # move the player
                if key == 'w':
                    newrow = newrow - 1
                elif key == 'a':
                    newcol = newcol - 1
                elif key == 's':
                    newrow = newrow + 1
                elif key == 'd':
                    newcol = newcol + 1
                
                if self.map[newrow][newcol] == '>':
                    # going down is just like starting over
                    self.setup()
                    return

                for j in range(1, self.monsters):
                    if self.loc[j].row == newrow and self.loc[j].col == newcol:
                        # player bumped a monster
                        if self.hp[j] > 0:
                            # and it was alive at the time
                            self.hp[j] = self.hp[j] - 1
                            if self.hp[j] == 0:
                                # now it is dead
                                self.map[self.loc[j].row][self.loc[j].col] = 'D'
            else:
                if self.hp[i] > 0:
                    # move a monster
                    newrow = newrow + rand(0, 3) - 1
                    newcol = newcol + rand(0, 3) - 1

                    if newrow == self.loc[0].row and newcol == self.loc[0].col:
                        # monster bumped the player
                        self.hp[0] = self.hp[0] - 1

                        if self.hp[0] == 0:
                            # monster killed the player
                            quit()
            
            # regardless, if an entity moved, the map needs to be updated
            if self.map[newrow][newcol] == '.':
                self.loc[i] = Point(newrow, newcol)
                self.map[newrow][newcol] = self.map[oldrow][oldcol]
                self.map[oldrow][oldcol] = '.'

start(MonsterCave())


