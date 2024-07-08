import "./food-chain" for FoodChain
import "wren-testie/testie" for Testie, Expect

Testie.test("FoodChain") { |do, skip|
  do.test("fly") {
    var actual = FoodChain.recite(1)
    var expected = [ "I know an old lady who swallowed a fly.", "I don't know why she swallowed the fly. Perhaps she'll die." ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("spider") {
    var actual = FoodChain.recite(2)
    var expected = [
      "I know an old lady who swallowed a spider.",
      "It wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("bird") {
    var actual = FoodChain.recite(3)
    var expected = [
      "I know an old lady who swallowed a bird.",
      "How absurd to swallow a bird!",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("cat") {
    var actual = FoodChain.recite(4)
    var expected = [
      "I know an old lady who swallowed a cat.",
      "Imagine that, to swallow a cat!",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("dog") {
    var actual = FoodChain.recite(5)
    var expected = [
      "I know an old lady who swallowed a dog.",
      "What a hog, to swallow a dog!",
      "She swallowed the dog to catch the cat.",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("goat") {
    var actual = FoodChain.recite(6)
    var expected = [
      "I know an old lady who swallowed a goat.",
      "Just opened her throat and swallowed a goat!",
      "She swallowed the goat to catch the dog.",
      "She swallowed the dog to catch the cat.",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("cow") {
    var actual = FoodChain.recite(7)
    var expected = [
      "I know an old lady who swallowed a cow.",
      "I don't know how she swallowed a cow!",
      "She swallowed the cow to catch the goat.",
      "She swallowed the goat to catch the dog.",
      "She swallowed the dog to catch the cat.",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("horse") {
    var actual = FoodChain.recite(8)
    var expected = [
      "I know an old lady who swallowed a horse.",
      "She's dead, of course!"
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("multiple verses") {
    var actual = FoodChain.recite(1, 3)
    var expected = [
      "I know an old lady who swallowed a fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a spider.",
      "It wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a bird.",
      "How absurd to swallow a bird!",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die."
    ]
    Expect.value(actual).toEqual(expected)
  }

  do.test("full song") {
    var actual = FoodChain.recite(1, 8)
    var expected = [
      "I know an old lady who swallowed a fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a spider.",
      "It wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a bird.",
      "How absurd to swallow a bird!",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a cat.",
      "Imagine that, to swallow a cat!",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a dog.",
      "What a hog, to swallow a dog!",
      "She swallowed the dog to catch the cat.",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a goat.",
      "Just opened her throat and swallowed a goat!",
      "She swallowed the goat to catch the dog.",
      "She swallowed the dog to catch the cat.",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a cow.",
      "I don't know how she swallowed a cow!",
      "She swallowed the cow to catch the goat.",
      "She swallowed the goat to catch the dog.",
      "She swallowed the dog to catch the cat.",
      "She swallowed the cat to catch the bird.",
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
      "She swallowed the spider to catch the fly.",
      "I don't know why she swallowed the fly. Perhaps she'll die.",
      "",
      "I know an old lady who swallowed a horse.",
      "She's dead, of course!"
    ]
    Expect.value(actual).toEqual(expected)
  }
}
