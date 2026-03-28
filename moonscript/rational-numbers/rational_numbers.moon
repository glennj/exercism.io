gcd = (a, b) ->
  b == 0 and a or gcd b, a % b
  
reduce = (r) ->
  {num, den} = r
  if num == 0 then return {0, 1}

  if den < 0 then num, den = -num, -den
  div = gcd num, den
  {num / div, den / div}

add = (r1, r2) ->
  {a, b} = r1
  {c, d} = r2
  reduce {a * d + c * b, b * d}

negate = (r) ->
  {num, den} = r
  {-num, den}

sub = (r1, r2) ->
  add r1, negate r2

mul = (r1, r2) ->
  {a, b} = r1
  {c, d} = r2
  reduce {a * c, b * d}

invert = (r) ->
  {num, den} = r
  {den, num}

div = (r1, r2) ->
  mul r1, invert r2

abs = (r) ->
  reduce {math.abs(r[1]), math.abs(r[2])}

exprational = (r, n) ->
  {a, b} = r
  if n < 0
    n = -n
    a, b = b, a
  reduce {a ^ n, b ^ n}

expreal = (x, r) ->
  {a, b} = r
  x ^ (a / b)

{ :reduce, :add, :sub, :mul, :div, :abs, :exprational, :expreal }
