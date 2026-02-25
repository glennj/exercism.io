{
  largest_product: (digits, span) ->
    assert span <= #digits, 'span must not exceed string length'
    assert span >= 0, 'span must not be negative'
    assert digits\match('^%d*$'), 'digits input must only contain digits'

    ds = [tonumber d for d in digits\gmatch '.']

    start = #ds - span + 1
    max = -1

    while start > 0
      prod = 1
      prod *= ds[i] for i = start, start + span - 1
      max = math.max prod, max
      start -= 1

    max
}
