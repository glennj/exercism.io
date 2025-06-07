FoodChain = require './food-chain'

describe 'Food Chain', ->
  it 'fly', ->
    expected = [
      "I know an old lady who swallowed a fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 1, 1).toEqual expected

  it 'spider', ->
    expected = [
      "I know an old lady who swallowed a spider."
      "It wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 2, 2).toEqual expected

  it 'bird', ->
    expected = [
      "I know an old lady who swallowed a bird."
      "How absurd to swallow a bird!"
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 3, 3).toEqual expected

  it 'cat', ->
    expected = [
      "I know an old lady who swallowed a cat."
      "Imagine that, to swallow a cat!"
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 4, 4).toEqual expected

  it 'dog', ->
    expected = [
      "I know an old lady who swallowed a dog."
      "What a hog, to swallow a dog!"
      "She swallowed the dog to catch the cat."
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 5, 5).toEqual expected
  
  it 'goat', ->
    expected = [
      "I know an old lady who swallowed a goat."
      "Just opened her throat and swallowed a goat!"
      "She swallowed the goat to catch the dog."
      "She swallowed the dog to catch the cat."
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 6, 6).toEqual expected

  it 'cow', ->
    expected = [
      "I know an old lady who swallowed a cow."
      "I don't know how she swallowed a cow!"
      "She swallowed the cow to catch the goat."
      "She swallowed the goat to catch the dog."
      "She swallowed the dog to catch the cat."
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 7, 7).toEqual expected

  it 'horse', ->
    expected = [
      "I know an old lady who swallowed a horse."
      "She's dead, of course!"
    ].join "\n"
    expect(FoodChain.recite 8, 8).toEqual expected

  it 'multiple verses', ->
    expected = [
      "I know an old lady who swallowed a fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a spider."
      "It wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a bird."
      "How absurd to swallow a bird!"
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ].join "\n"
    expect(FoodChain.recite 1, 3).toEqual expected

  it 'full song', ->
    expected = [
      "I know an old lady who swallowed a fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a spider."
      "It wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a bird."
      "How absurd to swallow a bird!"
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a cat."
      "Imagine that, to swallow a cat!"
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a dog."
      "What a hog, to swallow a dog!"
      "She swallowed the dog to catch the cat."
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a goat."
      "Just opened her throat and swallowed a goat!"
      "She swallowed the goat to catch the dog."
      "She swallowed the dog to catch the cat."
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a cow."
      "I don't know how she swallowed a cow!"
      "She swallowed the cow to catch the goat."
      "She swallowed the goat to catch the dog."
      "She swallowed the dog to catch the cat."
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die."
      ""
      "I know an old lady who swallowed a horse."
      "She's dead, of course!"
    ].join "\n"
    expect(FoodChain.recite 1, 8).toEqual expected

