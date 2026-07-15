import factors from require 'prime_factors'

describe 'prime-factors:', ->
  it 'no factors', ->
    result = factors 1
    expected = {}
    assert.are.same expected, result

  it 'prime number', ->
    result = factors 2
    expected = {2}
    assert.are.same expected, result

  it 'another prime number', ->
    result = factors 3
    expected = {3}
    assert.are.same expected, result

  it 'square of a prime', ->
    result = factors 9
    expected = {3, 3}
    assert.are.same expected, result

  it 'product of first prime', ->
    result = factors 4
    expected = {2, 2}
    assert.are.same expected, result

  it 'cube of a prime', ->
    result = factors 8
    expected = {2, 2, 2}
    assert.are.same expected, result

  it 'product of second prime', ->
    result = factors 27
    expected = {3, 3, 3}
    assert.are.same expected, result

  it 'product of third prime', ->
    result = factors 625
    expected = {5, 5, 5, 5}
    assert.are.same expected, result

  it 'product of first and second prime', ->
    result = factors 6
    expected = {2, 3}
    assert.are.same expected, result

  it 'product of primes and non-primes', ->
    result = factors 12
    expected = {2, 2, 3}
    assert.are.same expected, result

  it 'product of primes', ->
    result = factors 901255
    expected = {5, 17, 23, 461}
    assert.are.same expected, result

  it 'factors include a large prime', ->
    result = factors 93819012551
    expected = {11, 9539, 894119}
    assert.are.same expected, result

