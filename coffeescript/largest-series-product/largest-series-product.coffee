product = (ns) -> ns.reduce(((product, n) -> product * n), 1)

class LargestSeriesProduct
  @largestProduct: (digits, span) ->
    throw new Error 'span must not be negative' if span < 0
    throw new Error 'span must not exceed string length' if span > digits.length
    throw new Error 'digits input must only contain digits' if /\D/.test(digits)

    ds = (Number.parseInt(d) for d in digits)
    products = (product(ds[i ... i + span]) for i in [0 .. ds.length - span])
    Math.max products...
      

module.exports = LargestSeriesProduct
