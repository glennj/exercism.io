import "./palindrome-products" for PalindromeProducts
import "wren-testie/testie" for Testie, Expect

Testie.test("PalindromeProducts") { |do, skip|
  do.test("find the smallest palindrome from single digit factors") {
    var result = PalindromeProducts.smallest(1, 9)
    Expect.value(result.value).toEqual(1)
    Expect.value(result.factors).toEqual([[1, 1]])
  }

  do.test("find the largest palindrome from single digit factors") {
    var result = PalindromeProducts.largest(1, 9)
    Expect.value(result.value).toEqual(9)
    Expect.value(result.factors).toEqual([[1, 9], [3, 3]])
  }

  do.test("find the smallest palindrome from double digit factors") {
    var result = PalindromeProducts.smallest(10, 99)
    Expect.value(result.value).toEqual(121)
    Expect.value(result.factors).toEqual([[11, 11]])
  }

  do.test("find the largest palindrome from double digit factors") {
    var result = PalindromeProducts.largest(10, 99)
    Expect.value(result.value).toEqual(9009)
    Expect.value(result.factors).toEqual([[91, 99]])
  }

  do.test("find the smallest palindrome from triple digit factors") {
    var result = PalindromeProducts.smallest(100, 999)
    Expect.value(result.value).toEqual(10201)
    Expect.value(result.factors).toEqual([[101, 101]])
  }

  do.test("find the largest palindrome from triple digit factors") {
    var result = PalindromeProducts.largest(100, 999)
    Expect.value(result.value).toEqual(906609)
    Expect.value(result.factors).toEqual([[913, 993]])
  }

  do.test("find the smallest palindrome from four digit factors") {
    var result = PalindromeProducts.smallest(1000, 9999)
    Expect.value(result.value).toEqual(1002001)
    Expect.value(result.factors).toEqual([[1001, 1001]])
  }

  do.test("find the largest palindrome from four digit factors") {
    var result = PalindromeProducts.largest(1000, 9999)
    Expect.value(result.value).toEqual(99000099)
    Expect.value(result.factors).toEqual([[9901, 9999]])
  }

  do.test("empty result for smallest if no palindrome in the range") {
    var result = PalindromeProducts.smallest(1002, 1003)
    Expect.value(result.value).toEqual(null)
    Expect.value(result.factors).toEqual([])
  }

  do.test("empty result for largest if no palindrome in the range") {
    var result = PalindromeProducts.largest(15, 15)
    Expect.value(result.value).toEqual(null)
    Expect.value(result.factors).toEqual([])
  }

  Expect.that {
    PalindromeProducts.smallest(10000, 1)
  }.abortsWith("min must be <= max")

  Expect.that {
    PalindromeProducts.largest(2, 1)
  }.abortsWith("min must be <= max")

  do.test("smallest product does not use the smallest factor") {
    var result = PalindromeProducts.smallest(3215, 4000)
    Expect.value(result.value).toEqual(10988901)
    Expect.value(result.factors).toEqual([[3297, 3333]])
  }
}
