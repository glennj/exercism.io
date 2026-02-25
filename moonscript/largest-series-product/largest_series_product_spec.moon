import largest_product from require 'largest_series_product'

describe 'largest-series-product', ->
  it 'finds the largest product if span equals length', ->
    result = largest_product '29', 2
    assert.are.equal 18, result

  it 'can find the largest product of 2 with numbers in order', ->
    result = largest_product '0123456789', 2
    assert.are.equal 72, result

  it 'can find the largest product of 2', ->
    result = largest_product '576802143', 2
    assert.are.equal 48, result

  it 'can find the largest product of 3 with numbers in order', ->
    result = largest_product '0123456789', 3
    assert.are.equal 504, result

  it 'can find the largest product of 3', ->
    result = largest_product '1027839564', 3
    assert.are.equal 270, result

  it 'can find the largest product of 5 with numbers in order', ->
    result = largest_product '0123456789', 5
    assert.are.equal 15120, result

  it 'can get the largest product of a big number', ->
    result = largest_product '73167176531330624919225119674426574742355349194934', 6
    assert.are.equal 23520, result

  it 'reports zero if the only digits are zero', ->
    result = largest_product '0000', 2
    assert.are.equal 0, result

  it 'reports zero if all spans include zero', ->
    result = largest_product '99099', 3
    assert.are.equal 0, result

  it 'rejects span longer than string length', ->
    f = -> largest_product '123', 4
    assert.has.errors f, 'span must not exceed string length'

  it 'reports 1 for empty string and empty product (0 span)', ->
    -- There may be some confusion about whether this should be 1 or error.
    -- The reasoning for it being 1 is this:
    -- There is one 0-character string contained in the empty string.
    -- That's the empty string itself.
    -- The empty product is 1 (the identity for multiplication).
    -- Therefore LSP('', 0) is 1.
    -- It's NOT the case that LSP('', 0) takes max of an empty list.
    -- So there is no error.
    -- Compare against LSP('123', 4):
    -- There are zero 4-character strings in '123'.
    -- So LSP('123', 4) really DOES take the max of an empty list.
    -- So LSP('123', 4) errors and LSP('', 0) does NOT.
    result = largest_product '', 0
    assert.are.equal 1, result

  it 'reports 1 for nonempty string and empty product (0 span)', ->
    -- As above, there is one 0-character string in '123'.
    -- So again no error. It's the empty product, 1.
    result = largest_product '123', 0
    assert.are.equal 1, result

  it 'rejects empty string and nonzero span', ->
    f = -> largest_product '', 1
    assert.has.errors f, 'span must not exceed string length'

  it 'rejects invalid character in digits', ->
    f = -> largest_product '1234a5', 2
    assert.has.errors f, 'digits input must only contain digits'

  it 'rejects negative span', ->
    f = -> largest_product '12345', -1
    assert.has.errors f, 'span must not be negative'
