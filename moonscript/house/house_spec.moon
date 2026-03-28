House = require 'house'

describe 'house', ->
  it 'verse one - the house that jack built', ->
    result = House.recite 1, 1
    expected = {'This is the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse two - the malt that lay', ->
    result = House.recite 2, 2
    expected = {'This is the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse three - the rat that ate', ->
    result = House.recite 3, 3
    expected = {'This is the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse four - the cat that killed', ->
    result = House.recite 4, 4
    expected = {'This is the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse five - the dog that worried', ->
    result = House.recite 5, 5
    expected = {'This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse six - the cow with the crumpled horn', ->
    result = House.recite 6, 6
    expected = {'This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse seven - the maiden all forlorn', ->
    result = House.recite 7, 7
    expected = {'This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse eight - the man all tattered and torn', ->
    result = House.recite 8, 8
    expected = {'This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse nine - the priest all shaven and shorn', ->
    result = House.recite 9, 9
    expected = {'This is the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse 10 - the rooster that crowed in the morn', ->
    result = House.recite 10, 10
    expected = {'This is the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse 11 - the farmer sowing his corn', ->
    result = House.recite 11, 11
    expected = {'This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'verse 12 - the horse and the hound and the horn', ->
    result = House.recite 12, 12
    expected = {'This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.'}
    assert.are.same expected, result

  pending 'multiple verses', ->
    result = House.recite 4, 8
    expected = {
      'This is the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
    }
    assert.are.same expected, result

  pending 'full rhyme', ->
    result = House.recite 1, 12
    expected = {
      'This is the house that Jack built.',
      'This is the malt that lay in the house that Jack built.',
      'This is the rat that ate the malt that lay in the house that Jack built.',
      'This is the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
      'This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.',
    }
    assert.are.same expected, result
