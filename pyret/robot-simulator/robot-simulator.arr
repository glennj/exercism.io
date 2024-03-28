use context essentials2020 # Don't delete this line when using Pyret on Exercism

provide-types *

data Robot:
  | robot(x, y, bearing)
    with:

    method move(self, instructions :: String) -> Robot:
      string-explode(instructions).foldl(
        lam(instruction, robbie):
          ask:
            | instruction == 'R' then: robbie.turn-right()
            | instruction == 'L' then: robbie.turn-left()
            | instruction == 'A' then: robbie.advance()
            | otherwise: raise('invalid instruction')
          end
        end,
        self)
    end,

    method turn-right(self) -> Robot:
      robot(
        self.x,
        self.y,
        ask:
          | self.bearing == 'north' then: 'east'
          | self.bearing == 'east'  then: 'south'
          | self.bearing == 'south' then: 'west'
          | self.bearing == 'west'  then: 'north'
        end)
    end,

    method turn-left(self) -> Robot:
      robot(
        self.x,
        self.y,
        ask:
          | self.bearing == 'north' then: 'west'
          | self.bearing == 'east'  then: 'north'
          | self.bearing == 'south' then: 'east'
          | self.bearing == 'west'  then: 'south'
        end)
    end,

    method advance(self) -> Robot:
      ask:
        | self.bearing == 'north' then: robot(self.x,     self.y + 1, self.bearing)
        | self.bearing == 'east'  then: robot(self.x + 1, self.y,     self.bearing)
        | self.bearing == 'south' then: robot(self.x,     self.y - 1, self.bearing)
        | self.bearing == 'west'  then: robot(self.x - 1, self.y,     self.bearing)
      end
    end
end
