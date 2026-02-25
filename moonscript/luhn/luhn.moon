doubled =
  [false]: {[0]: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
  [true]:  {[0]: 0, 2, 4, 6, 8, 1, 3, 5, 7, 9}


luhn_sum = (clean) ->
  digits = [tonumber d for d in clean\gmatch '.']
  double = false
  sum = 0
  for idx = #digits, 1, -1
    sum += doubled[double][digits[idx]]
    double = not double
  sum


{
  is_valid: (input) ->
    clean = input\gsub '%s', ''
    return false if #clean < 2 or clean\match '%D'
    luhn_sum(clean) % 10 == 0
}
