import "./proverb" for Proverb
import "wren-testie/testie" for Testie, Expect

Testie.test("Proverb") { |do, skip|
  do.test("zero pieces") {
    var strings = [
    ]
    var expected = [
    ]
    Expect.value(Proverb.recite(strings)).toEqual(expected)
  }

  skip.test("one piece") {
    var strings = [
      "nail",
    ]
    var expected = [
      "And all for the want of a nail.",
    ]
    Expect.value(Proverb.recite(strings)).toEqual(expected)
  }

  skip.test("two pieces") {
    var strings = [
      "nail",
      "shoe",
    ]
    var expected = [
      "For want of a nail the shoe was lost.",
      "And all for the want of a nail.",
    ]
    Expect.value(Proverb.recite(strings)).toEqual(expected)
  }

  skip.test("three pieces") {
    var strings = [
      "nail",
      "shoe",
      "horse",
    ]
    var expected = [
      "For want of a nail the shoe was lost.",
      "For want of a shoe the horse was lost.",
      "And all for the want of a nail.",
    ]
    Expect.value(Proverb.recite(strings)).toEqual(expected)
  }

  skip.test("full proverb") {
    var strings = [
      "nail",
      "shoe",
      "horse",
      "rider",
      "message",
      "battle",
      "kingdom",
    ]
    var expected = [
      "For want of a nail the shoe was lost.",
      "For want of a shoe the horse was lost.",
      "For want of a horse the rider was lost.",
      "For want of a rider the message was lost.",
      "For want of a message the battle was lost.",
      "For want of a battle the kingdom was lost.",
      "And all for the want of a nail.",
    ]
    Expect.value(Proverb.recite(strings)).toEqual(expected)
  }

  skip.test("four pieces modernized") {
    var strings = [
      "pin",
      "gun",
      "soldier",
      "battle",
    ]
    var expected = [
      "For want of a pin the gun was lost.",
      "For want of a gun the soldier was lost.",
      "For want of a soldier the battle was lost.",
      "And all for the want of a pin.",
    ]
    Expect.value(Proverb.recite(strings)).toEqual(expected)
  }
}
