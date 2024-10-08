class IsbnVerifier
  @isValid: (isbn) ->
    sum = 0
    idx = 0
    for c in [isbn...]
      continue if c is '-'
      d = Number.parseInt c
      if not Number.isNaN d
        sum += (10 - idx) * d
      else if idx is 9 and c is 'X'
        sum += 10
      else
        return false
      idx++
    return idx is 10 and sum % 11 is 0
      

module.exports = IsbnVerifier
