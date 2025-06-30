Lsp = require './largest-series-product'

describe 'Largest Series Product', ->
  it 'finds the largest product if span equals length', ->
    result = Lsp.largestProduct '29', 2
    expect(result).toEqual 18

  it 'can find the largest product of 2 with numbers in order', ->
    result = Lsp.largestProduct '0123456789', 2
    expect(result).toEqual 72

  it 'can find the largest product of 2', ->
    result = Lsp.largestProduct '576802143', 2
    expect(result).toEqual 48
  
  it 'can find the largest product of 3 with numbers in order', ->
    result = Lsp.largestProduct '0123456789', 3
    expect(result).toEqual 504

  it 'can find the largest product of 3', ->
    result = Lsp.largestProduct '1027839564', 3
    expect(result).toEqual 270

  it 'can find the largest product of 5 with numbers in order', ->
    result = Lsp.largestProduct '0123456789', 5
    expect(result).toEqual 15120
  
  it 'can get the largest product of a big number', ->
    result = Lsp.largestProduct '73167176531330624919225119674426574742355349194934', 6
    expect(result).toEqual 23520

  it 'reports zero if the only digits are zero', ->
    result = Lsp.largestProduct '0000', 2
    expect(result).toEqual 0

  it 'reports zero if all spans include zero', ->
    result = Lsp.largestProduct '99099', 3
    expect(result).toEqual 0

  it 'rejects span longer than string length', ->
    expect ->
      Lsp.largestProduct '123', 4
    .toThrow new Error 'span must not exceed string length'

  it 'reports 1 for empty string and empty product (0 span)', ->
    result = Lsp.largestProduct '', 0
    expect(result).toEqual 1

  it 'reports 1 for nonempty string and empty product (0 span)', ->
    result = Lsp.largestProduct '123', 0
    expect(result).toEqual 1

  it 'rejects empty string and nonzero span', ->
    expect ->
      Lsp.largestProduct '', 1
    .toThrow new Error 'span must not exceed string length'

  it 'rejects invalid character in digits', ->
    expect ->
      Lsp.largestProduct '1234a5', 2
    .toThrow new Error 'digits input must only contain digits'

  it 'rejects negative span', ->
    expect ->
      Lsp.largestProduct '12345', -1
    .toThrow new Error 'span must not be negative'
