-- based on Steven Fuerst's obfuscated, 512-byte roguelike "Monster Cave".
MonsterCave = {
  rows = 25,
  cols = 80,
  rooms = 50,
  monsters = 10,
  map = {},
  loc = {},
  hp = {}
}

function MonsterCave:setup()
  local row = 0
  local col = 0
  for row = 0, self.rows do
    self.map[row] = {}
    for col = 0, self.cols do
      self.map[row][col] = '#'
    end
  end

  -- make a messy cave by opening out a lot of rectangles
  local i
  for i = 0, self.rooms do
    local height = rand(3, 6)
    local width = rand(1, 25)
    local top = rand(1, self.rows - height - 1)
    local left = rand(1, self.cols - width - 1)
    local bottom = top + height
    local right = left + width
    for row = top, bottom do
      for col = left, right do
        self.map[row][col] = '.'
      end
    end
  end

  -- place entities (player and monsters)
  for i = 0, self.monsters + 1 do
    while true do
      local row = rand(1, self.rows - 1)
      local col = rand(1, self.cols - 1)
      if self.map[row][col] == '.' then
        self.loc[i] = { row = row, col = col }
        if i == 0 then
          -- the player
          self.map[row][col] = '@'
          self.hp[i] = 5
        else
          -- a monster
          self.map[row][col] = 'M'
          self.hp[i] = 2
        end
        break
      end
    end
  end

  -- place the stairway
  while true do
    local row = rand(1, self.rows - 1)
    local col = rand(1, self.cols - 1)
    if self.map[row][col] == '.' then
      self.map[row][col] = '>'
      break
    end
  end
end

function MonsterCave:draw()
  local row = 0
  local col = 0
  for row = 0, self.rows do
    move(row, 0)
    for col = 0, self.cols do
      addch(self.map[row][col])
    end
  end

  -- put the cursor on the player
  move(self.loc[0].row, self.loc[0].col)
end


function MonsterCave:update(key)
  if key == ERR then
    return
  end

  local i = 0
  for i = 0, self.monsters + 1 do
    local oldrow = self.loc[i].row
    local newrow = oldrow
    local oldcol = self.loc[i].col
    local newcol = oldcol
    if i == 0 then

      -- move the player
      if key == string.byte('w') then
        newrow = newrow - 1
      elseif key == string.byte('a') then
        newcol = newcol - 1
      elseif key == string.byte('s') then
        newrow = newrow + 1
      elseif key == string.byte('d') then
        newcol = newcol + 1
      end

      if self.map[newrow][newcol] == '>' then
        -- going down is just like starting over! :P
        self:setup()
        return
      end

      local j = 0
      for j = 1, self.monsters do
        if self.loc[j].row == newrow and self.loc[j].col == newcol then
          -- player stepped on a monster
          if self.hp[j] > 0 then
             -- monster was alive at the time
             self.hp[j] = self.hp[j] - 1
             if self.hp[j] == 0 then
               -- now it is dead...
               self.map[self.loc[j].row][self.loc[j].col] = 'D'
             end
          end
        end
      end
    else

      if self.hp[i] > 0 then
        -- move a monster
        newrow = newrow + rand(0, 3) - 1
        newcol = newcol + rand(0, 3) - 1

        if newrow == self.loc[0].row and newcol == self.loc[0].col then
          -- monster stepped on the player
          self.hp[0] = self.hp[0] - 1

          if self.hp[0] == 0 then
            -- monster killed the player
	    quit()
          end
        end
      end
    end

    -- regardless, if an entity moved, the map needs to be updated
    if self.map[newrow][newcol] == '.' then
      self.loc[i] = { row = newrow, col = newcol }
      self.map[newrow][newcol] = self.map[oldrow][oldcol]
      self.map[oldrow][oldcol] = '.'
    end
  end
end

start(MonsterCave)

