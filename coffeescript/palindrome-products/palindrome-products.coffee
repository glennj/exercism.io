class PalindromeProducts
  constructor: ({@maxFactor, @minFactor = 1}) ->

  smallest: -> @solve from: @minFactor ** 2, to: @maxFactor ** 2, step: +1
  largest:  -> @solve from: @maxFactor ** 2, to: @minFactor ** 2, step: -1

  solve: ({from, to, step}) ->
    throw new Error 'min must be <= max' unless @minFactor <= @maxFactor

    result = {value: null, factors: []}
    for prod in [from..to] by step
      if prod.isPalindrome()
        factors = @boundedFactors prod
        if factors.length > 0
          result.value = prod
          result.factors = factors
          break
    result

  boundedFactors: (n) ->
    ( [f, n / f] \
      for f in [@minFactor .. Math.floor Math.sqrt n] \
      when n % f == 0 and @minFactor <= n / f <= @maxFactor )

module.exports = PalindromeProducts


# "Borrowed" from Erik's solution
# https://exercism.org/tracks/coffeescript/exercises/palindrome-products/solutions/ErikSchierboom

Number::isPalindrome = -> 
  n = this
  rev = 0
  while n > 0
    dig = n % 10
    rev = rev * 10 + dig
    n = n // 10
  this + 0 == rev + 0   # why is adding zero needed??

