import abbreviate from require 'acronym'

describe 'acronym', ->
  it 'basic', ->
    result = abbreviate 'Portable Network Graphics'
    assert.are.equal 'PNG', result

  it 'lowercase words', ->
    result = abbreviate 'Ruby on Rails'
    assert.are.equal 'ROR', result

  it 'punctuation', ->
    result = abbreviate 'First In, First Out'
    assert.are.equal 'FIFO', result

  it 'all caps word', ->
    result = abbreviate 'GNU Image Manipulation Program'
    assert.are.equal 'GIMP', result

  it 'punctuation without whitespace', ->
    result = abbreviate 'Complementary metal-oxide semiconductor'
    assert.are.equal 'CMOS', result

  it 'very long abbreviation', ->
    result = abbreviate 'Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me'
    assert.are.equal 'ROTFLSHTMDCOALM', result

  it 'consecutive delimiters', ->
    result = abbreviate 'Something - I made up from thin air'
    assert.are.equal 'SIMUFTA', result

  it 'apostrophes', ->
    result = abbreviate "Halley's Comet"
    assert.are.equal 'HC', result

  it 'underscore emphasis', ->
    result = abbreviate 'The Road _Not_ Taken'
    assert.are.equal 'TRNT', result
