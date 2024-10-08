class SumOfMultiples
  @sum: (factors, limit) ->
    multiples = new Set()
    multiples.add m for m in [f ... limit] by f for f in factors when f > 0
    Array.from(multiples).reduce ((sum, m) -> sum + m), 0

module.exports = SumOfMultiples
