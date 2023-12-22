use context essentials2020

#provide-types *

provide:
  data Clock
end

import equality as E

data Clock:
  | clock(hours :: NumInteger, minutes :: NumInteger)

sharing:
  method add(self, minutes :: NumInteger) -> Clock:
    clock(self.hours, self.minutes + minutes).normalize()
  end,

  method subtract(self, minutes :: NumInteger) -> Clock:
    self.add(-1 * minutes)
  end,

  method normalize(self) -> Clock:
    mins = self.normalized-minutes()
    clock(num-floor(mins / 60), num-modulo(mins, 60))
  end,

  method normalized-minutes(self) -> Number:
    num-modulo((self.hours * 60) + self.minutes, 24 * 60)
  end,

  method _equals(self, other :: Clock, _) -> E.EqualityResult:
    # The callback argument after other is ignored for this exercise
    {a; b} = {self.normalized-minutes(); other.normalized-minutes()}
    ask:
      | a == b then: E.Equal
      | otherwise: E.NotEqual("not equal", a, b)
    end
  end,

  method to-string(self) -> String:
    padded = lam(n):
      p = "0" + num-to-string(n)
      l = string-length(p)
      string-substring(p, l - 2, l)
    end
    padded(self.hours) + ":" + padded(self.minutes)
  end
end
