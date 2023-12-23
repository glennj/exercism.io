use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide:
  data Character
end

data Character:
  | character(
      strength :: NumInteger,
      dexterity :: NumInteger,
      constitution :: NumInteger,
      intelligence :: NumInteger,
      wisdom :: NumInteger,
      charisma :: NumInteger
    )
    with:
      method get-hitpoints(self):
        self.modifier(self.constitution) + 10
      end

  | blank-character()
    with:
      method ability(self):
        doc: "The sum of top 3 of 4 six-sided dice"
        {sum; min} = for fold({s; m} from {0; 6}, _ from range(0, 4)):
          die = 1 + num-random(6)
          {s + die; num-min(m, die)}
        end
        sum - min
      end,

      method randomize-stats(self):
        character(
          self.ability(), # strength
          self.ability(), # dexterity
          self.ability(), # constitution
          self.ability(), # intelligence
          self.ability(), # wisdom
          self.ability()  # charisma
        )
      end

  sharing:
    method modifier(self, points):
      num-floor((points - 10) / 2)
    end
end

