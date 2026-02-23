ResistorColorDuo = require 'resistor_color_duo'

describe 'resistor-color-duo', ->
  it 'Brown and black', ->
    result = ResistorColorDuo.value 'brown', 'black'
    assert.are.same 10, result

  it 'Blue and grey', ->
    result = ResistorColorDuo.value 'blue', 'grey'
    assert.are.same 68, result

  it 'Yellow and violet', ->
    result = ResistorColorDuo.value 'yellow', 'violet'
    assert.are.same 47, result

  it 'White and red', ->
    result = ResistorColorDuo.value 'white', 'red'
    assert.are.same 92, result

  it 'Orange and orange', ->
    result = ResistorColorDuo.value 'orange', 'orange'
    assert.are.same 33, result

  it 'Ignore additional colors', ->
    result = ResistorColorDuo.value 'green', 'brown', 'orange'
    assert.are.same 51, result

  it 'Black and brown, one-digit', ->
    result = ResistorColorDuo.value 'black', 'brown'
    assert.are.same 1, result
