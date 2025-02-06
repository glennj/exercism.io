import "./house" for House
import "wren-testie/testie" for Testie, Expect

Testie.test("House") { |do, skip|
  do.test("verse one - the house that jack built") {
    var expected = [
      "This is the house that Jack built.",
    ]
    Expect.value(House.recite(1, 1)).toEqual(expected)
  }

  do.test("verse two - the malt that lay") {
    var expected = [
      "This is the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(2, 2)).toEqual(expected)
  }

  do.test("verse three - the rat that ate") {
    var expected = [
      "This is the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(3, 3)).toEqual(expected)
  }

  do.test("verse four - the cat that killed") {
    var expected = [
      "This is the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(4, 4)).toEqual(expected)
  }

  do.test("verse five - the dog that worried") {
    var expected = [
      "This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(5, 5)).toEqual(expected)
  }

  do.test("verse six - the cow with the crumpled horn") {
    var expected = [
      "This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(6, 6)).toEqual(expected)
  }

  do.test("verse seven - the maiden all forlorn") {
    var expected = [
      "This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(7, 7)).toEqual(expected)
  }

  do.test("verse eight - the man all tattered and torn") {
    var expected = [
      "This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(8, 8)).toEqual(expected)
  }

  do.test("verse nine - the priest all shaven and shorn") {
    var expected = [
      "This is the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(9, 9)).toEqual(expected)
  }

  do.test("verse 10 - the rooster that crowed in the morn") {
    var expected = [
      "This is the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(10, 10)).toEqual(expected)
  }

  do.test("verse 11 - the farmer sowing his corn") {
    var expected = [
      "This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(11, 11)).toEqual(expected)
  }

  do.test("verse 12 - the horse and the hound and the horn") {
    var expected = [
      "This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(12, 12)).toEqual(expected)
  }

  do.test("multiple verses") {
    var expected = [
      "This is the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(4, 8)).toEqual(expected)
  }

  do.test("full rhyme") {
    var expected = [
      "This is the house that Jack built.",
      "This is the malt that lay in the house that Jack built.",
      "This is the rat that ate the malt that lay in the house that Jack built.",
      "This is the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
      "This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
    ]
    Expect.value(House.recite(1, 12)).toEqual(expected)
  }
}
