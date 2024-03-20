use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: to-roman end

fun to-roman(number):
  rec helper = lam(dec, rom):
    appender = lam(base, tens, fives, ones):
      ask:
        | dec >= (10 * base) then: helper(dec - (10 * base), rom + tens)
        | dec >= ( 9 * base) then: helper(dec + ( 1 * base), rom + ones)
        | dec >= ( 5 * base) then: helper(dec - ( 5 * base), rom + fives)
        | dec >= ( 4 * base) then: helper(dec + ( 1 * base), rom + ones)
        | otherwise:               helper(dec - ( 1 * base), rom + ones)
      end
    end

    ask:
      | dec >= 400 then: appender(100, "M", "D", "C")
      | dec >=  40 then: appender(10,  "C", "L", "X")
      | dec >=   1 then: appender(1,   "X", "V", "I")
      | otherwise: rom
    end
  end

  helper(number, "")
end

#fun to-roman-take1(number):
#  rec helper = lam(dec, rom):
#    ask:
#      | dec >= 1000 then: helper(dec - 1000, rom +  "M")
#      | dec >=  900 then: helper(dec -  900, rom + "CM")
#      | dec >=  500 then: helper(dec -  500, rom +  "D")
#      | dec >=  400 then: helper(dec -  400, rom + "CD")
#      | dec >=  100 then: helper(dec -  100, rom +  "C")
#      | dec >=   90 then: helper(dec -   90, rom + "XC")
#      | dec >=   50 then: helper(dec -   50, rom +  "L")
#      | dec >=   40 then: helper(dec -   40, rom + "XL")
#      | dec >=   10 then: helper(dec -   10, rom +  "X")
#      | dec >=    9 then: helper(dec -    9, rom + "IX")
#      | dec >=    5 then: helper(dec -    5, rom +  "V")
#      | dec >=    4 then: helper(dec -    4, rom + "IV")
#      | dec >=    1 then: helper(dec -    1, rom +  "I")
#      | otherwise: rom
#    end
#  end
#  helper(number, "")
#end
