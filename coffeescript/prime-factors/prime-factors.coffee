class PrimeFactors
  @factors: (value) ->
    fs = []
    f = 2
    while f * f <= value
      if value % f == 0
        fs.push f
        value /= f
      else
        f++
    fs.push value if value > 1
    fs

module.exports = PrimeFactors
