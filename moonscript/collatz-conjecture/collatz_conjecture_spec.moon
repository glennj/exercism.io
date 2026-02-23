CollatzConjecture = require 'collatz_conjecture'

describe 'collatz-conjecture', ->
  it 'zero steps for one', ->
    result = CollatzConjecture.steps 1
    assert.are.equal 0, result

  it 'divide if even', ->
    result = CollatzConjecture.steps 16
    assert.are.equal 4, result

  it 'even and odd steps', ->
    result = CollatzConjecture.steps 12
    assert.are.equal 9, result

  it 'large number of even and odd steps', ->
    result = CollatzConjecture.steps 1000000
    assert.are.equal 152, result

  it 'zero is an error', ->
    f = -> CollatzConjecture.steps 0
    assert.has.errors f, 'Only positive integers are allowed'

  it 'negative value is an error', ->
    f = -> CollatzConjecture.steps -15
    assert.has.errors f, 'Only positive integers are allowed'
