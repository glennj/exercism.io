class Say
  @say: (n) -> 
    throw new Error 'input out of range'   if n < 0
    return saySmall(n)                     if n < 100
    return sayCompound(n, 100, 'hundred')  if n < 1e3
    return sayCompound(n, 1e3, 'thousand') if n < 1e6
    return sayCompound(n, 1e6, 'million')  if n < 1e9
    return sayCompound(n, 1e9, 'billion')  if n < 1e12
    throw new Error 'input out of range'


saySmall = (n) -> words[n] or words[n - n % 10] + "-" + words[n % 10]

sayCompound = (n, base, word) ->
  rem = n % base
  Say.say((n - rem) / base) + " " + word + (if rem then " " + Say.say(rem) else "")

words = [
  'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven',
  'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen',
  'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'
]
words[20] = 'twenty'; words[30] = 'thirty';  words[40] = 'forty';  words[50] = 'fifty'
words[60] = 'sixty';  words[70] = 'seventy'; words[80] = 'eighty'; words[90] = 'ninety'

module.exports = Say
