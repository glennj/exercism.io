class PerfectNumbers
  @classify: (number) ->
    throw new Error 'Classification is only possible for positive integers.' if number < 1

    switch Math.sign (@aliquotSum(number) - number)
      when -1 then 'deficient'
      when  0 then 'perfect'
      when  1 then 'abundant'

  @aliquotSum = (n) ->
    sum = 0
    for f in [1 .. (Math.floor Math.sqrt n)]
      if n % f == 0
        sum += f
        if f != n / f
          sum += n / f
    sum - n


module.exports = PerfectNumbers
