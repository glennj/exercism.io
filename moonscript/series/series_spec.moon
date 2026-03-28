import slices from require 'series'

describe 'series', ->
  it 'slices of one from one', ->
    result = [s for s in slices '1', 1]
    expected = {'1'}
    assert.are.same expected, result

  pending 'slices of one from two', ->
    result = [s for s in slices '12', 1]
    expected = {'1', '2'}
    assert.are.same expected, result

  pending 'slices of two', ->
    result = [s for s in slices '35', 2]
    expected = {'35'}
    assert.are.same expected, result

  pending 'slices of two overlap', ->
    result = [s for s in slices '9142', 2]
    expected = {'91', '14', '42'}
    assert.are.same expected, result

  pending 'slices can include duplicates', ->
    result = [s for s in slices '777777', 3]
    expected = {'777', '777', '777', '777'}
    assert.are.same expected, result

  pending 'slices of a long series', ->
    result = [s for s in slices '918493904243', 5]
    expected = {'91849', '18493', '84939', '49390', '93904', '39042', '90424', '04243'}
    assert.are.same expected, result

  pending 'slice length is too large', ->
    f = -> slices '12345', 6
    assert.has_error f, 'slice length cannot be greater than series length'

  pending 'slice length is way too large', ->
    f = -> slices '12345', 42
    assert.has_error f, 'slice length cannot be greater than series length'

  pending 'slice length cannot be zero', ->
    f = -> slices '12345', 0
    assert.has_error f, 'slice length cannot be zero'

  pending 'slice length cannot be negative', ->
    f = -> slices '123', -1
    assert.has_error f, 'slice length cannot be negative'

  pending 'empty series is invalid', ->
    f = -> slices '', 1
    assert.has_error f, 'series cannot be empty'
