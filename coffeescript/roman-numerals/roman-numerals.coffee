class RomanNumerals
  @convert: (number) ->
    roman = ''
    for r, d of mapping
      while number >= d
        roman += r
        number -= d
    roman

mapping =
  M: 1000
  CM: 900
  D:  500
  CD: 400
  C:  100
  XC:  90
  L:   50
  XL:  40
  X:   10
  IX:   9
  V:    5
  IV:   4
  I:    1
    
module.exports = RomanNumerals
