ResistorColorTrio = require './resistor-color-trio'

describe 'ResistorColorTrio', ->
  it 'Orange and orange and black', ->
    results = ResistorColorTrio.label ['orange', 'orange', 'black']
    expect(results).toEqual '33 ohms'

  it 'Blue and grey and brown', ->
    results = ResistorColorTrio.label ['blue', 'grey', 'brown']
    expect(results).toEqual '680 ohms'

  it 'Red and black and red', ->
    results = ResistorColorTrio.label ['red', 'black', 'red']
    expect(results).toEqual '2 kiloohms'

  it 'Green and brown and orange', ->
    results = ResistorColorTrio.label ['green', 'brown', 'orange']
    expect(results).toEqual '51 kiloohms'

  it 'Yellow and violet and yellow', ->
    results = ResistorColorTrio.label ['yellow', 'violet', 'yellow']
    expect(results).toEqual '470 kiloohms'

  it 'Blue and violet and blue', ->
    results = ResistorColorTrio.label ['blue', 'violet', 'blue']
    expect(results).toEqual '67 megaohms'

  it 'Minimum possible value', ->
    results = ResistorColorTrio.label ['black', 'black', 'black']
    expect(results).toEqual '0 ohms'

  it 'Maximum possible value', ->
    results = ResistorColorTrio.label ['white', 'white', 'white']
    expect(results).toEqual '99 gigaohms'

  it 'First two colors make an invalid octal number', ->
    results = ResistorColorTrio.label ['black', 'grey', 'black']
    expect(results).toEqual '8 ohms'

  it 'Ignore extra colors', ->
    results = ResistorColorTrio.label ['blue', 'green', 'yellow', 'orange']
    expect(results).toEqual '650 kiloohms'
