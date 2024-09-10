primes = [2, 3, 5]  # "prime" the pump. har, har

isPrime = (n) ->
  return true if n == 2
  return false if n % 2 == 0
  limit = Math.floor Math.sqrt n
  for p in primes
    # assume limit <= largest currently known prime
    return true if p > limit
    return false if n % p == 0

nextPrime = ->
  p = primes[primes.length - 1] + 2
  until isPrime p
    p += 2
  primes.push p


class NthPrime
  @nth: (nthPrime) ->
    throw new Error 'Prime is not possible' if nthPrime < 1

    while primes.length < nthPrime
      nextPrime()

    primes[nthPrime - 1]

module.exports = NthPrime
