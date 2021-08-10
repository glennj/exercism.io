import "./affine-cipher" for AffineCipher
import "wren-testie/testie" for Testie, Expect

Testie.test("Affine cipher") { |do, skip|
  do.describe("encode") {
    do.test("encode yes") {
      Expect.value(AffineCipher.encode("yes", { "a": 5, "b": 7 })).toBe("xbt")
    }

    do.test("encode no") {
      Expect.value(AffineCipher.encode("no", { "a": 15, "b": 18 })).toBe("fu")
    }

    do.test("encode OMG") {
      Expect.value(AffineCipher.encode("OMG", { "a": 21, "b": 3 })).toBe("lvz")
    }

    do.test("encode O M G") {
      Expect.value(AffineCipher.encode("O M G", { "a": 25, "b": 47 })).toBe("hjp")
    }

    do.test("encode mindblowingly") {
      Expect.value(AffineCipher.encode("mindblowingly", { "a": 11, "b": 15 })).toBe("rzcwa gnxzc dgt")
    }

    do.test("encode numbers") {
      Expect.value(AffineCipher.encode("Testing,1 2 3, testing.", { "a": 3, "b": 4 })).toBe(
        "jqgjc rw123 jqgjc rw"
      )
    }

    do.test("encode deep thought") {
      Expect.value(AffineCipher.encode("Truth is fiction.", { "a": 5, "b": 17 })).toBe(
        "iynia fdqfb ifje"
      )
    }

    do.test("encode all the letters") {
      Expect.value(
        AffineCipher.encode("The quick brown fox jumps over the lazy dog.", { "a": 17, "b": 33 })
      ).toBe("swxtj npvyk lruol iejdc blaxk swxmh qzglf")
    }

    do.test("encode with a not coprime to m") {
      Expect.that {
        AffineCipher.encode("This is a test.", { "a": 6, "b": 17 })
      }.abortsWith("a and m must be coprime.")
    }
  }
  do.describe("decode") {
    do.test("decode exercism") {
      Expect.value(AffineCipher.decode("tytgn fjr", { "a": 3, "b": 7 })).toBe("exercism")
    }

    do.test("decode a sentence") {
      Expect.value(
        AffineCipher.decode("qdwju nqcro muwhn odqun oppmd aunwd o", { "a": 19, "b": 16 })
      ).toBe("anobstacleisoftenasteppingstone")
    }

    do.test("decode numbers") {
      Expect.value(AffineCipher.decode("odpoz ub123 odpoz ub", { "a": 25, "b": 7 })).toBe(
        "testing123testing"
      )
    }

    do.test("decode all the letters") {
      Expect.value(
        AffineCipher.decode("swxtj npvyk lruol iejdc blaxk swxmh qzglf", { "a": 17, "b": 33 })
      ).toBe("thequickbrownfoxjumpsoverthelazydog")
    }

    do.test("decode with no spaces in input") {
      Expect.value(
        AffineCipher.decode("swxtjnpvyklruoliejdcblaxkswxmhqzglf", { "a": 17, "b": 33 })
      ).toBe("thequickbrownfoxjumpsoverthelazydog")
    }

    do.test("decode with too many spaces") {
      Expect.value(AffineCipher.decode("vszzm    cly   yd cg    qdp", { "a": 15, "b": 16 })).toBe(
        "jollygreengiant"
      )
    }

    do.test("decode with a not coprime to m") {
      Expect.that {
        AffineCipher.decode("Test", { "a": 13, "b": 5 })
      }.abortsWith("a and m must be coprime.")
    }
  }
}
