{
  is_valid: (input) ->
    input = input\gsub '-', ''
    return false if not input\match "^#{'%d'\rep 9}[%dX]$"

    idx = #input
    sum = 0
    for digit in input\gmatch '.'
      sum += idx * (digit == 'X' and 10 or tonumber digit)
      idx -= 1

    sum % 11 == 0
}
