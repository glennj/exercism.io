class ArmstrongNumbers
  @isArmstrongNumber: (number) -> number is @armstrongSum number

  @armstrongSum: (n) ->
    len = 1 + Math.floor Math.log10 n
    sum = 0
    while n > 0
      [digit, n] = [n % 10, n // 10]
      sum += digit ** len
    return sum
  
module.exports = ArmstrongNumbers
