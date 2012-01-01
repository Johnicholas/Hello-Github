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
    # TODO
  end

  def draw
    LittleCurses.addch('h')
    LittleCurses.addch('i')
  end

  def update(key)
    # LittleCurses.quit
    # TODO
  end
end

LittleCurses.start(MonsterCave.new)

