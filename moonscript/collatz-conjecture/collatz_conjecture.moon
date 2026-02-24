next_collatz = (n) -> n % 2 == 0 and n / 2 or 3 * n + 1

do_steps = (s, n) ->
  return s if n == 1
  do_steps s + 1, next_collatz n

steps = (number) ->
  assert number > 0, 'Only positive integers are allowed'
  do_steps 0, number

{ :steps }
