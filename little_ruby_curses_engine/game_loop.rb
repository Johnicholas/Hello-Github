# based on Steven Fuerst's obfuscated, 512-byte roguelike "Monster Cave".

include LittleCurses

class MonsterCave
  def initialize
    @rows = 25
    @cols = 80
    @rooms = 50
    @monsters = 10
    @map = {}
    @loc = {}
    @hp = {}
  end

  def setup
    for row in 0..@rows
      @map[row] = {}
      for col in 0..@cols
        @map[row][col] = '#'
      end
    end

    # make a messy cave by opening out a lot of rectangles
    for i in 0..@rooms
      height = rand 3, 6
      width = rand 1, 25
      top = rand 1, @rows - height - 1
      left = rand 1, @cols - width - 1
      bottom = top + height
      right = left + width
      for row in top..bottom
        for col in left..right
          @map[row][col] = '.'
        end
      end
    end
    
    # place entities (player and monsters)
    for i in 0..@monsters + 1
      while true
        row = rand 1, @rows - 1
        col = rand 1, @cols - 1
        if @map[row][col] == '.'
          @loc[i] = { :row => row, :col => col }
          if i == 0
            # the player
            @map[row][col] = '@'
            @hp[i] = 5
          else
            # a monster
            @map[row][col] = 'M'
            @hp[i] = 2
          end
          break
        end
      end
    end
    
    # place the stairway
    while true
      row = rand 1, @rows - 1
      col = rand 1, @cols - 1
      if @map[row][col] == '.'
        @map[row][col] = '>'
        break
      end
    end
  end

  def draw
    for row in 0..@rows
      LittleCurses.move row, 0
      for col in 0..@cols
        LittleCurses.addch @map[row][col]
      end
    end

    # put the cursor on the player
    LittleCurses.move @loc[0][:row], @loc[0][:col]
  end

  def update(key)
    # TODO
    # if key == ERR
    #   return
    # else
    key = key.chr

    for i in 0..@monsters+1
      oldrow = @loc[i][:row]
      newrow = oldrow
      oldcol = @loc[i][:col]
      newcol = oldcol
      if i == 0

        # move the player
        if key == 'w'
          newrow = newrow - 1
        elsif key == 'a'
          newcol = newcol - 1
        elsif key == 's'
          newrow = newrow + 1
        elsif key == 'd'
          newcol = newcol + 1
        end

        if @map[newrow][newcol] == '>'
          # going down is just like starting over
          setup
          return
        end

        for j in 1..@monsters
          if @loc[j][:row] == newrow and @loc[j][:col] == newcol
            # player bumped a monster
            if @hp[j] > 0
              # and it was alive at the time
              @hp[j] = @hp[j] - 1
              if @hp[j] == 0
                  # now it is dead
                  @map[@loc[j][:row]][@loc[j][:col]] = 'D'
              end
            end
          end
        end
      else
        if @hp[i] > 0
          # move a monster
          newrow = newrow + rand(0, 3) - 1
          newcol = newcol + rand(0, 3) - 1
          if newrow == @loc[0][:row] and newcol == @loc[0][:col]
            # monster bumped the player
            @hp[0] = @hp[0] - 1
            if @hp[0] == 0
              LittleCurses.quit
            end
          end
        end
      end

      # regardless, if an entity moved, the map needs to be updated
      if @map[newrow][newcol] == '.'
        @loc[i] = { :row => newrow, :col => newcol }
        @map[newrow][newcol] = @map[oldrow][oldcol]
        @map[oldrow][oldcol] = '.'
      end
    end
  end
end

LittleCurses.start MonsterCave.new

