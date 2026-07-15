import slices from require 'series'

describe 'series:', ->
  it 'slices of one from one', ->
    result = [slice for slice in slices '1', 1]
    expected = {'1'}
    assert.are.same expected, result

  it 'slices of one from two', ->
    result = [slice for slice in slices '12', 1]
    expected = {'1', '2'}
    assert.are.same expected, result

  it 'slices of two', ->
    result = [slice for slice in slices '35', 2]
    expected = {'35'}
    assert.are.same expected, result

  it 'slices of two overlap', ->
    result = [slice for slice in slices '9142', 2]
    expected = {'91', '14', '42'}
    assert.are.same expected, result

  it 'slices can include duplicates', ->
    result = [slice for slice in slices '777777', 3]
    expected = {'777', '777', '777', '777'}
    assert.are.same expected, result

  it 'slices of a long series', ->
    result = [slice for slice in slices '918493904243', 5]
    expected = {'91849', '18493', '84939', '49390', '93904', '39042', '90424', '04243'}
    assert.are.same expected, result

  it 'slice length is too large', ->
    f = -> slices '12345', 6
    assert.has_error f, 'slice length cannot be greater than series length'

  it 'slice length is way too large', ->
    f = -> slices '12345', 42
    assert.has_error f, 'slice length cannot be greater than series length'

  it 'slice length cannot be zero', ->
    f = -> slices '12345', 0
    assert.has_error f, 'slice length cannot be zero'

  it 'slice length cannot be negative', ->
    f = -> slices '123', -1
    assert.has_error f, 'slice length cannot be negative'

  it 'empty series is invalid', ->
    f = -> slices '', 1
    assert.has_error f, 'series cannot be empty'

