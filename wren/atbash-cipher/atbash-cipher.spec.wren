import "./atbash-cipher" for Atbash
import "wren-testie/testie" for Testie, Expect

Testie.test("Atbash Cipher") { |do, skip|
  do.describe("encode") {
    do.test("encode yes") {
      Expect.value(Atbash.encode("yes")).toEqual("bvh")
    }

    do.test("encode no") {
      Expect.value(Atbash.encode("no")).toEqual("ml")
    }

    do.test("encode OMG") {
      Expect.value(Atbash.encode("OMG")).toEqual("lnt")
    }

    do.test("encode spaces") {
      Expect.value(Atbash.encode("O M G")).toEqual("lnt")
    }

    do.test("encode mindblowingly") {
      Expect.value(Atbash.encode("mindblowingly")).toEqual("nrmwy oldrm tob")
    }

    do.test("encode numbers") {
      var messageToEncode = "Testing,1 2 3, testing."
      var expected = "gvhgr mt123 gvhgr mt"
      Expect.value(Atbash.encode(messageToEncode)).toEqual(expected)
    }

    do.test("encode deep thought") {
      var messageToEncode = "Truth is fiction."
      var expected = "gifgs rhurx grlm"
      Expect.value(Atbash.encode(messageToEncode)).toEqual(expected)
    }

    do.test("encode all the letters") {
      var messageToEncode = "The quick brown fox jumps over the lazy dog."
      var expected = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
      Expect.value(Atbash.encode(messageToEncode)).toEqual(expected)
    }
  }

  do.describe("decode") {
    do.test("decode exercism") {
      Expect.value(Atbash.decode("vcvix rhn")).toEqual("exercism")
    }

    do.test("decode a sentence") {
      var messageToDecode = "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
      var expected = "anobstacleisoftenasteppingstone"
      Expect.value(Atbash.decode(messageToDecode)).toEqual(expected)
    }

    do.test("decode numbers") {
      Expect.value(Atbash.decode("gvhgr mt123 gvhgr mt")).toEqual("testing123testing")
    }

    do.test("decode all the letters") {
      var messageToDecode = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
      var expected = "thequickbrownfoxjumpsoverthelazydog"
      Expect.value(Atbash.decode(messageToDecode)).toEqual(expected)
    }

    do.test("decode with too many spaces") {
      Expect.value(Atbash.decode("vc vix    r hn")).toEqual("exercism")
    }

    do.test("decode with no spaces") {
      var messageToDecode = "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
      var expected = "anobstacleisoftenasteppingstone"
      Expect.value(Atbash.decode(messageToDecode)).toEqual(expected)
    }
  }
}
