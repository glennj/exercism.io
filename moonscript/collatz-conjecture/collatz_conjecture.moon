steps = (number) ->
  assert number > 0, 'Only positive integers are allowed'
  
  next_collatz = (n) -> n % 2 == 0 and n / 2 or 3 * n + 1

  do_steps = (n, s) ->
    return s if n == 1
    do_steps next_collatz(n), s + 1

  do_steps number, 0

{ :steps }
