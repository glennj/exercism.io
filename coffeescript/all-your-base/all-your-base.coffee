allYourBase = ({inputBase, digits, outputBase}) ->
  throw new Error 'input base must be >= 2' unless inputBase >= 2
  throw new Error 'output base must be >= 2' unless outputBase >= 2
  throw new Error 'all digits must satisfy 0 <= d < input base' \
    unless digits.every((d) -> 0 <= d < inputBase)

  decimal = digits.reduce(((dec, digit) -> dec * inputBase + digit), 0)
  return [0] if decimal is 0

  outputDigits = []
  while decimal > 0
    outputDigits.unshift(decimal % outputBase)
    decimal //= outputBase
  outputDigits
  
module.exports = allYourBase
