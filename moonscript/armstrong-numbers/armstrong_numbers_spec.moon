ArmstrongNumbers = require 'armstrong_numbers'

describe 'armstrong-numbers:', ->
  it 'Zero is an Armstrong number', ->
    result = ArmstrongNumbers.is_armstrong 0
    assert.is_true result

  it 'Single-digit numbers are Armstrong numbers', ->
    result = ArmstrongNumbers.is_armstrong 5
    assert.is_true result

  it 'There are no two-digit Armstrong numbers', ->
    result = ArmstrongNumbers.is_armstrong 10
    assert.is_false result

  it 'Three-digit number that is an Armstrong number', ->
    result = ArmstrongNumbers.is_armstrong 153
    assert.is_true result

  it 'Three-digit number that is not an Armstrong number', ->
    result = ArmstrongNumbers.is_armstrong 100
    assert.is_false result

  it 'Four-digit number that is an Armstrong number', ->
    result = ArmstrongNumbers.is_armstrong 9474
    assert.is_true result

  it 'Four-digit number that is not an Armstrong number', ->
    result = ArmstrongNumbers.is_armstrong 9475
    assert.is_false result

  it 'Seven-digit number that is an Armstrong number', ->
    result = ArmstrongNumbers.is_armstrong 9926315
    assert.is_true result

  it 'Seven-digit number that is not an Armstrong number', ->
    result = ArmstrongNumbers.is_armstrong 9926314
    assert.is_false result

