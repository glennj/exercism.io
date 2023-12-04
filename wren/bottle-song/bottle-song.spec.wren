import "./bottle-song" for BottleSong
import "wren-testie/testie" for Testie, Expect

Testie.test("BottleSong.recite()") { |do, skip|
  do.test("verse -> single verse -> first generic verse") {
    var expected = [
      "Ten green bottles hanging on the wall,",
      "Ten green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be nine green bottles hanging on the wall.",
    ]
    Expect.value(BottleSong.recite(10, 1)).toEqual(expected)
  }

  do.test("verse -> single verse -> last generic verse") {
    var expected = [
      "Three green bottles hanging on the wall,",
      "Three green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be two green bottles hanging on the wall.",
    ]
    Expect.value(BottleSong.recite(3, 1)).toEqual(expected)
  }

  do.test("verse -> single verse -> verse with 2 bottles") {
    var expected = [
      "Two green bottles hanging on the wall,",
      "Two green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be one green bottle hanging on the wall.",
    ]
    Expect.value(BottleSong.recite(2, 1)).toEqual(expected)
  }

  do.test("verse -> single verse -> verse with 1 bottle") {
    var expected = [
      "One green bottle hanging on the wall,",
      "One green bottle hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be no green bottles hanging on the wall.",
    ]
    Expect.value(BottleSong.recite(1, 1)).toEqual(expected)
  }

  do.test("lyrics -> multiple verses -> first two verses") {
    var expected = [
      "Ten green bottles hanging on the wall,",
      "Ten green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be nine green bottles hanging on the wall.",
      "",
      "Nine green bottles hanging on the wall,",
      "Nine green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be eight green bottles hanging on the wall.",
    ]
    Expect.value(BottleSong.recite(10, 2)).toEqual(expected)
  }

  do.test("lyrics -> multiple verses -> last three verses") {
    var expected = [
      "Three green bottles hanging on the wall,",
      "Three green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be two green bottles hanging on the wall.",
      "",
      "Two green bottles hanging on the wall,",
      "Two green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be one green bottle hanging on the wall.",
      "",
      "One green bottle hanging on the wall,",
      "One green bottle hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be no green bottles hanging on the wall.",
    ]
    Expect.value(BottleSong.recite(3, 3)).toEqual(expected)
  }

  do.test("lyrics -> multiple verses -> all verses") {
    var expected = [
      "Ten green bottles hanging on the wall,",
      "Ten green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be nine green bottles hanging on the wall.",
      "",
      "Nine green bottles hanging on the wall,",
      "Nine green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be eight green bottles hanging on the wall.",
      "",
      "Eight green bottles hanging on the wall,",
      "Eight green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be seven green bottles hanging on the wall.",
      "",
      "Seven green bottles hanging on the wall,",
      "Seven green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be six green bottles hanging on the wall.",
      "",
      "Six green bottles hanging on the wall,",
      "Six green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be five green bottles hanging on the wall.",
      "",
      "Five green bottles hanging on the wall,",
      "Five green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be four green bottles hanging on the wall.",
      "",
      "Four green bottles hanging on the wall,",
      "Four green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be three green bottles hanging on the wall.",
      "",
      "Three green bottles hanging on the wall,",
      "Three green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be two green bottles hanging on the wall.",
      "",
      "Two green bottles hanging on the wall,",
      "Two green bottles hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be one green bottle hanging on the wall.",
      "",
      "One green bottle hanging on the wall,",
      "One green bottle hanging on the wall,",
      "And if one green bottle should accidentally fall,",
      "There'll be no green bottles hanging on the wall.",
    ]
    Expect.value(BottleSong.recite(10, 10)).toEqual(expected)
  }
}
