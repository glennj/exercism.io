TwoFer = require 'two_fer'

describe 'two-fer', ->
  it 'no name given', ->
    result = TwoFer.two_fer!
    assert.equal 'One for you, one for me.', result

  it 'a name given', ->
    result = TwoFer.two_fer 'Alice'
    assert.equal 'One for Alice, one for me.', result

  it 'another name given', ->
    result = TwoFer.two_fer 'Bob'
    assert.equal 'One for Bob, one for me.', result

