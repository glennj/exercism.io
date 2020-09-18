proc reverse*(str: string): string =
  var rev = ""

  #[ This looks neater, but it would do a lot of copying of strings

      for c in str:
        rev = c & rev
  ]#

  #for i in countdown(str.len - 1, 0):
  for i in countdown(str.high, str.low):
    rev.add(str[i])

  return rev
