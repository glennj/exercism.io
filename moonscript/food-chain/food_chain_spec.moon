FoodChain = require 'food_chain'

describe 'food-chain', ->
  it 'fly', ->
    result = FoodChain.recite 1, 1
    expected = {'I know an old lady who swallowed a fly.', "I don't know why she swallowed the fly. Perhaps she'll die."}
    assert.are.equal table.concat(expected, "\n"), result

  it 'spider', ->
    result = FoodChain.recite 2, 2
    expected = {
      'I know an old lady who swallowed a spider.',
      'It wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
    }
    assert.are.equal table.concat(expected, "\n"), result

  it 'bird', ->
    result = FoodChain.recite 3, 3
    expected = {
      'I know an old lady who swallowed a bird.',
      'How absurd to swallow a bird!',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
    }
    assert.are.equal table.concat(expected, "\n"), result

  it 'cat', ->
    result = FoodChain.recite 4, 4
    expected = {
      'I know an old lady who swallowed a cat.',
      'Imagine that, to swallow a cat!',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
    }
    assert.are.equal table.concat(expected, "\n"), result

  it 'dog', ->
    result = FoodChain.recite 5, 5
    expected = {
      'I know an old lady who swallowed a dog.',
      'What a hog, to swallow a dog!',
      'She swallowed the dog to catch the cat.',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
    }
    assert.are.equal table.concat(expected, "\n"), result

  it 'goat', ->
    result = FoodChain.recite 6, 6
    expected = {
      'I know an old lady who swallowed a goat.',
      'Just opened her throat and swallowed a goat!',
      'She swallowed the goat to catch the dog.',
      'She swallowed the dog to catch the cat.',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
    }
    assert.are.equal table.concat(expected, "\n"), result

  it 'cow', ->
    result = FoodChain.recite 7, 7
    expected = {
      'I know an old lady who swallowed a cow.',
      "I don't know how she swallowed a cow!",
      'She swallowed the cow to catch the goat.',
      'She swallowed the goat to catch the dog.',
      'She swallowed the dog to catch the cat.',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
    }
    assert.are.equal table.concat(expected, "\n"), result

  it 'horse', ->
    result = FoodChain.recite 8, 8
    expected = {'I know an old lady who swallowed a horse.', "She's dead, of course!"}
    assert.are.equal table.concat(expected, "\n"), result

  it 'multiple verses', ->
    result = FoodChain.recite 1, 3
    expected = {
      'I know an old lady who swallowed a fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a spider.',
      'It wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a bird.',
      'How absurd to swallow a bird!',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
    }
    assert.are.equal table.concat(expected, "\n"), result

  it 'full song', ->
    result = FoodChain.recite 1, 8
    expected = {
      'I know an old lady who swallowed a fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a spider.',
      'It wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a bird.',
      'How absurd to swallow a bird!',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a cat.',
      'Imagine that, to swallow a cat!',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a dog.',
      'What a hog, to swallow a dog!',
      'She swallowed the dog to catch the cat.',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a goat.',
      'Just opened her throat and swallowed a goat!',
      'She swallowed the goat to catch the dog.',
      'She swallowed the dog to catch the cat.',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a cow.',
      "I don't know how she swallowed a cow!",
      'She swallowed the cow to catch the goat.',
      'She swallowed the goat to catch the dog.',
      'She swallowed the dog to catch the cat.',
      'She swallowed the cat to catch the bird.',
      'She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.',
      'She swallowed the spider to catch the fly.',
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      '',
      'I know an old lady who swallowed a horse.',
      "She's dead, of course!",
    }
    assert.are.equal table.concat(expected, "\n"), result
