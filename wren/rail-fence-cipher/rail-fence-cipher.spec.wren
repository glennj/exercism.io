import "./rail-fence-cipher" for RailFenceCipher
import "wren-testie/testie" for Testie, Expect

Testie.test("RailFenceCipher") { |do, skip|
  do.describe("encode") {
    do.test("encode with two rails") {
      var message = "XOXOXOXOXOXOXOXOXO"
      var encoded = "XXXXXXXXXOOOOOOOOO"
      Expect.value(RailFenceCipher.encode(message, 2)).toEqual(encoded)
    }

    do.test("encode with three rails") {
      var message = "WEAREDISCOVEREDFLEEATONCE"
      var encoded = "WECRLTEERDSOEEFEAOCAIVDEN"
      Expect.value(RailFenceCipher.encode(message, 3)).toEqual(encoded)
    }

    do.test("encode with ending in the middle") {
      var message = "EXERCISES"
      var encoded = "ESXIEECSR"
      Expect.value(RailFenceCipher.encode(message, 4)).toEqual(encoded)
    }
  }

  do.describe("decode") {
    do.test("decode with three rails") {
      var message = "TEITELHDVLSNHDTISEIIEA"
      var decoded = "THEDEVILISINTHEDETAILS"
      Expect.value(RailFenceCipher.decode(message, 3)).toEqual(decoded)
    }

    do.test("decode with five rails") {
      var message = "EIEXMSMESAORIWSCE"
      var decoded = "EXERCISMISAWESOME"
      Expect.value(RailFenceCipher.decode(message, 5)).toEqual(decoded)
    }

    do.test("decode with six rails") {
      var message = "133714114238148966225439541018335470986172518171757571896261"
      var decoded = "112358132134558914423337761098715972584418167651094617711286"
      Expect.value(RailFenceCipher.decode(message, 6)).toEqual(decoded)
    }
  }
}
