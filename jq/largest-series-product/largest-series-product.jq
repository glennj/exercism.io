# It would be interesting to convert .digits to a number _first_
# and do the rest of the work with arithmetic.
# However, jq cannot represent big integers accurately.

# input: a string, intended to be digits-only
# output: an array of single-digit numbers, or die
def todigits(errmsg):
  try (. / "" | map(tonumber))
  catch (errmsg | halt_error)
;

def assert(cond; msg): if cond then . else (msg | halt_error) end;

# input: map of {digits: [array of single-digit numbers], span: anInt}
# output: array of arrays of single-digit numbers
def spans:
  . as {$digits, $span}
  | reduce range(($digits | length) - $span + 1) as $i ([]; . + [$digits[$i : $i + $span]])
;

# input: array of numbers
# output: the product of said numbers
def product: reduce .[] as $n (1; . * $n);

.digits |= todigits("digits input must only contain digits")
| assert(.span >= 0; "span must not be negative")
| assert(.span <= (.digits | length); "span must not exceed string length")
| spans
| map(product)
| max
