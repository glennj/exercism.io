import "./rotational-cipher" for RotationalCipher
import "wren-testie/testie" for Testie, Expect

Testie.test("RotationalCipher") { |do, skip|
  do.test("rotate a by 0, same output as input") {
    var phrase = "a"
    var expect = "a"
    Expect.value(RotationalCipher.rotate(phrase, 0)).toEqual(expect)
  }

  do.test("rotate a by 1") {
    var phrase = "a"
    var expect = "b"
    Expect.value(RotationalCipher.rotate(phrase, 1)).toEqual(expect)
  }

  do.test("rotate a by 26, same output as input") {
    var phrase = "a"
    var expect = "a"
    Expect.value(RotationalCipher.rotate(phrase, 26)).toEqual(expect)
  }

  do.test("rotate m by 13") {
    var phrase = "m"
    var expect = "z"
    Expect.value(RotationalCipher.rotate(phrase, 13)).toEqual(expect)
  }

  do.test("rotate n by 13 with wrap around alphabet") {
    var phrase = "n"
    var expect = "a"
    Expect.value(RotationalCipher.rotate(phrase, 13)).toEqual(expect)
  }

  do.test("rotate capital letters") {
    var phrase = "OMG"
    var expect = "TRL"
    Expect.value(RotationalCipher.rotate(phrase, 5)).toEqual(expect)
  }

  do.test("rotate spaces") {
    var phrase = "O M G"
    var expect = "T R L"
    Expect.value(RotationalCipher.rotate(phrase, 5)).toEqual(expect)
  }

  do.test("rotate numbers") {
    var phrase = "Testing 1 2 3 testing"
    var expect = "Xiwxmrk 1 2 3 xiwxmrk"
    Expect.value(RotationalCipher.rotate(phrase, 4)).toEqual(expect)
  }

  do.test("rotate punctuation") {
    var phrase = "Let's eat, Grandma!"
    var expect = "Gzo'n zvo, Bmviyhv!"
    Expect.value(RotationalCipher.rotate(phrase, 21)).toEqual(expect)
  }

  do.test("rotate all letters") {
    var phrase = "The quick brown fox jumps over the lazy dog."
    var expect = "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
    Expect.value(RotationalCipher.rotate(phrase, 13)).toEqual(expect)
  }
}
