ResistorColorTrio = require 'resistor_color_trio'

describe 'resistor-color-trio', ->
  it 'Orange and orange and black', ->
    result = ResistorColorTrio.label 'orange', 'orange', 'black'
    assert.are.equal '33 ohms', result

  it 'Blue and grey and brown', ->
    result = ResistorColorTrio.label 'blue', 'grey', 'brown'
    assert.are.equal '680 ohms', result

  it 'Red and black and red', ->
    result = ResistorColorTrio.label 'red', 'black', 'red'
    assert.are.equal '2 kiloohms', result

  it 'Green and brown and orange', ->
    result = ResistorColorTrio.label 'green', 'brown', 'orange'
    assert.are.equal '51 kiloohms', result

  it 'Yellow and violet and yellow', ->
    result = ResistorColorTrio.label 'yellow', 'violet', 'yellow'
    assert.are.equal '470 kiloohms', result

  it 'Blue and violet and blue', ->
    result = ResistorColorTrio.label 'blue', 'violet', 'blue'
    assert.are.equal '67 megaohms', result

  it 'Minimum possible value', ->
    result = ResistorColorTrio.label 'black', 'black', 'black'
    assert.are.equal '0 ohms', result

  it 'Maximum possible value', ->
    result = ResistorColorTrio.label 'white', 'white', 'white'
    assert.are.equal '99 gigaohms', result

  it 'First two colors make an invalid octal number', ->
    result = ResistorColorTrio.label 'black', 'grey', 'black'
    assert.are.equal '8 ohms', result

  it 'Ignore extra colors', ->
    result = ResistorColorTrio.label 'blue', 'green', 'yellow', 'orange'
    assert.are.equal '650 kiloohms', result
