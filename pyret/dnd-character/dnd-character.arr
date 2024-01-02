use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide-types *

import math as M

data Character:
  | blank-character()
    with:
    method randomize-stats(self) -> Character:
        character(
          self.ability(),    #  strength 
          self.ability(),    #  dexterity 
          self.ability(),    #  constitution 
          self.ability(),    #  intelligence 
          self.ability(),    #  wisdom 
          self.ability()     #  charisma 
        )
      end

  | character(strength, dexterity, constitution, intelligence, wisdom, charisma)
    with:
      method get-hitpoints(self):
        10 + self.modifier(self.constitution)
      end

sharing:
  method ability(self) -> NumInteger:
    dice = range(0, 4).map({(_): num-random(6) + 1})
    M.sum(dice) - M.min(dice)
  end,

  method modifier(self, value :: NumInteger) -> NumInteger:
     num-floor((value - 10) / 2)
  end
end
