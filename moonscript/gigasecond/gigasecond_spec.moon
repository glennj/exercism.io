Gigasecond = require 'gigasecond'

describe 'gigasecond', ->
  it 'date only specification of time', ->
    result = Gigasecond.add '2011-04-25'
    expected = '2043-01-01T01:46:40'
    assert.are.equal expected, result

  it 'second test for date only specification of time', ->
    result = Gigasecond.add '1977-06-13'
    expected = '2009-02-19T01:46:40'
    assert.are.equal expected, result

  it 'third test for date only specification of time', ->
    result = Gigasecond.add '1959-07-19'
    expected = '1991-03-27T01:46:40'
    assert.are.equal expected, result

  it 'full time specified', ->
    result = Gigasecond.add '2015-01-24T22:00:00'
    expected = '2046-10-02T23:46:40'
    assert.are.equal expected, result

  it 'full time with day roll-over', ->
    result = Gigasecond.add '2015-01-24T23:59:59'
    expected = '2046-10-03T01:46:39'
    assert.are.equal expected, result

