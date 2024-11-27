use "int-utils.sml";

fun armstrongSum(n: int): int =
  let
    val width = intWidth n
    fun sum'(0, sum) = sum
      | sum'(n, sum) = sum'(n div 10, sum + intPow(n mod 10, width))
  in
    sum'(n, 0)
  end

fun isArmstrongNumber(number: int): bool =
  number = armstrongSum number
