primes = {2, 3}

is_prime = (candidate) ->
  for p in *primes
    if p * p > candidate  then break
    if candidate % p == 0 then return false
  true

next_prime = ->
  n = primes[#primes] + 2
  while not is_prime n do n += 2
  table.insert primes, n


{
  prime: (n) ->
    assert n > 0, 'there is no zeroth prime'
    while #primes < n do next_prime!
    primes[n]
}
