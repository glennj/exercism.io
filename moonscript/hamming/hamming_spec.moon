hamming = require 'hamming'

describe 'hamming', ->
  it 'empty strands', ->
    result = hamming.distance '', ''
    assert.are.equal 0, result

  it 'single letter identical strands', ->
    result = hamming.distance 'A', 'A'
    assert.are.equal 0, result

  it 'single letter different strands', ->
    result = hamming.distance 'G', 'T'
    assert.are.equal 1, result

  it 'long identical strands', ->
    result = hamming.distance 'GGACTGAAATCTG', 'GGACTGAAATCTG'
    assert.are.equal 0, result

  it 'long different strands', ->
    result = hamming.distance 'GGACGGATTCTG', 'AGGACGGATTCT'
    assert.are.equal 9, result

  it 'disallow first strand longer', ->
    f = -> hamming.distance 'AATG', 'AAA'
    assert.has.error f, 'strands must be of equal length'

  it 'disallow second strand longer', ->
    f = -> hamming.distance 'ATA', 'AGTG'
    assert.has.error f, 'strands must be of equal length'

  it 'disallow empty first strand', ->
    f = -> hamming.distance '', 'G'
    assert.has.error f, 'strands must be of equal length'

  it 'disallow empty second strand', ->
    f = -> hamming.distance 'G', ''
    assert.has.error f, 'strands must be of equal length'
