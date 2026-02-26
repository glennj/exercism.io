-- translation of this lovely recursive solution
-- https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

SMALL = {
  [0]: 'zero', 'one', 'two', 'three', 'four', 'five', 'six',
  'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve',
  'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen',
  'eighteen', 'nineteen',
}

XTY = {
  [20]: 'twenty', [30]: 'thirty',  [40]: 'forty',  [50]: 'fifty',
  [60]: 'sixty',  [70]: 'seventy', [80]: 'eighty', [90]: 'ninety',
}


say_small = (n) ->
  SMALL[n] or XTY[n] or XTY[n - n%10] .. '-' .. SMALL[n%10]


local say

say_compound = (n, base, word) ->
  quot, rem = n // base, n % base
  said = "#{say quot} #{word}"
  if rem > 0 then said ..= " #{say rem}"
  said


say = (n) ->
  assert 0 <= n and n < 1e12, 'input out of range'

  if     n < 100 then say_small n
  elseif n < 1e3 then say_compound n, 100, 'hundred'
  elseif n < 1e6 then say_compound n, 1e3, 'thousand'
  elseif n < 1e9 then say_compound n, 1e6, 'million'
  else                say_compound n, 1e9, 'billion'


{ in_english: say }
