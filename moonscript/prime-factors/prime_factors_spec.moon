import factors from require 'prime_factors'

describe 'prime-factors', ->
  it 'no factors', ->
    expected = {}
    result = factors 1
    assert.are.same expected, result

  pending 'prime number', ->
    expected = {2}
    result = factors 2
    assert.are.same expected, result

  pending 'another prime number', ->
    expected = {3}
    result = factors 3
    assert.are.same expected, result

  pending 'square of a prime', ->
    expected = {3, 3}
    result = factors 9
    assert.are.same expected, result

  pending 'product of first prime', ->
    expected = {2, 2}
    result = factors 4
    assert.are.same expected, result

  pending 'cube of a prime', ->
    expected = {2, 2, 2}
    result = factors 8
    assert.are.same expected, result

  pending 'product of second prime', ->
    expected = {3, 3, 3}
    result = factors 27
    assert.are.same expected, result

  pending 'product of third prime', ->
    expected = {5, 5, 5, 5}
    result = factors 625
    assert.are.same expected, result

  pending 'product of first and second prime', ->
    expected = {2, 3}
    result = factors 6
    assert.are.same expected, result

  pending 'product of primes and non-primes', ->
    expected = {2, 2, 3}
    result = factors 12
    assert.are.same expected, result

  pending 'product of primes', ->
    expected = {5, 17, 23, 461}
    result = factors 901255
    assert.are.same expected, result

  pending 'factors include a large prime', ->
    expected = {11, 9539, 894119}
    result = factors 93819012551
    assert.are.same expected, result
