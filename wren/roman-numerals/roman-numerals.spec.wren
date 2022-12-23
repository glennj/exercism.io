import "./roman-numerals" for Number
import "wren-testie/testie" for Testie, Expect

Testie.test("Roman Numerals") { |do, skip|
  do.test("converts 1") { Expect.value(Number.toRoman(1)).toEqual("I") }
  skip.test("converts 2") { Expect.value(Number.toRoman(2)).toEqual("II") }
  skip.test("converts 3") { Expect.value(Number.toRoman(3)).toEqual("III") }
  skip.test("converts 4") { Expect.value(Number.toRoman(4)).toEqual("IV") }
  skip.test("converts 5") { Expect.value(Number.toRoman(5)).toEqual("V") }
  skip.test("converts 6") { Expect.value(Number.toRoman(6)).toEqual("VI") }
  skip.test("converts 9") { Expect.value(Number.toRoman(9)).toEqual("IX") }
  skip.test("converts 27") { Expect.value(Number.toRoman(27)).toEqual("XXVII") }
  skip.test("converts 48") { Expect.value(Number.toRoman(48)).toEqual("XLVIII") }
  skip.test("converts 49") { Expect.value(Number.toRoman(49)).toEqual("XLIX") }
  skip.test("converts 59") { Expect.value(Number.toRoman(59)).toEqual("LIX") }
  skip.test("converts 93") { Expect.value(Number.toRoman(93)).toEqual("XCIII") }
  skip.test("converts 141") { Expect.value(Number.toRoman(141)).toEqual("CXLI") }
  skip.test("converts 163") { Expect.value(Number.toRoman(163)).toEqual("CLXIII") }
  skip.test("converts 402") { Expect.value(Number.toRoman(402)).toEqual("CDII") }
  skip.test("converts 575") { Expect.value(Number.toRoman(575)).toEqual("DLXXV") }
  skip.test("converts 911") { Expect.value(Number.toRoman(911)).toEqual("CMXI") }
  skip.test("converts 1024") { Expect.value(Number.toRoman(1024)).toEqual("MXXIV") }
  skip.test("converts 3000") { Expect.value(Number.toRoman(3000)).toEqual("MMM") }
}
