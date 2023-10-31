import "./roman-numerals" for Number
import "wren-testie/testie" for Testie, Expect

Testie.test("Roman Numerals") { |do, skip|
  do.test("converts 1") { Expect.value(Number.toRoman(1)).toEqual("I") }
  do.test("converts 2") { Expect.value(Number.toRoman(2)).toEqual("II") }
  do.test("converts 3") { Expect.value(Number.toRoman(3)).toEqual("III") }
  do.test("converts 4") { Expect.value(Number.toRoman(4)).toEqual("IV") }
  do.test("converts 5") { Expect.value(Number.toRoman(5)).toEqual("V") }
  do.test("converts 6") { Expect.value(Number.toRoman(6)).toEqual("VI") }
  do.test("converts 9") { Expect.value(Number.toRoman(9)).toEqual("IX") }
  do.test("converts 27") { Expect.value(Number.toRoman(27)).toEqual("XXVII") }
  do.test("converts 48") { Expect.value(Number.toRoman(48)).toEqual("XLVIII") }
  do.test("converts 49") { Expect.value(Number.toRoman(49)).toEqual("XLIX") }
  do.test("converts 59") { Expect.value(Number.toRoman(59)).toEqual("LIX") }
  do.test("converts 93") { Expect.value(Number.toRoman(93)).toEqual("XCIII") }
  do.test("converts 141") { Expect.value(Number.toRoman(141)).toEqual("CXLI") }
  do.test("converts 163") { Expect.value(Number.toRoman(163)).toEqual("CLXIII") }
  do.test("converts 402") { Expect.value(Number.toRoman(402)).toEqual("CDII") }
  do.test("converts 575") { Expect.value(Number.toRoman(575)).toEqual("DLXXV") }
  do.test("converts 911") { Expect.value(Number.toRoman(911)).toEqual("CMXI") }
  do.test("converts 1024") { Expect.value(Number.toRoman(1024)).toEqual("MXXIV") }
  do.test("converts 3000") { Expect.value(Number.toRoman(3000)).toEqual("MMM") }
}
