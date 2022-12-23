import "wren-testie/testie" for Testie, Expect
import "./simple-cipher" for SimpleCipher

Testie.test("Simple Cipher") { |do, skip|
  do.describe("Random key cipher") {
    do.test("Key is made only of lowercase letters") {
      var cipher = SimpleCipher.new()
      Expect.value(cipher.key.count).toBeGreaterThanOrEqual(100)

      var alphabet = "abcdefghijklmnopqrstuvwxyz"
      for (char in cipher.key) {
        Expect.value(alphabet.contains(char)).toBe(true)
      }
    }

    skip.test("Can encode") {
      var cipher = SimpleCipher.new()
      var plaintext = "aaaaaaaaaa"
      var encoded = cipher.encode(plaintext)
      var expected = cipher.key[0...plaintext.count]
      Expect.value(encoded).toEqual(expected)
    }

    skip.test("Can decode") {
      var cipher = SimpleCipher.new()
      var plaintext = "aaaaaaaaaa"
      var ciphertext = cipher.key[0...plaintext.count]
      var decoded = cipher.decode(ciphertext)
      Expect.value(decoded).toEqual(plaintext)
    }

    skip.test("Is reversible") {
      var cipher = SimpleCipher.new()
      var plaintext = "abdefghij"
      var encoded = cipher.encode(plaintext)
      var decoded = cipher.decode(encoded)
      Expect.value(decoded).toEqual(plaintext)
    }
  }

  do.describe("Substitution cipher") {
    skip.test("Can encode") {
      var key = "abcdefghij"
      var cipher = SimpleCipher.new(key)
      var plaintext = "aaaaaaaaaa"
      Expect.value(cipher.encode(plaintext)).toEqual(key)
    }

    skip.test("Can decode") {
      var key = "abcdefghij"
      var cipher = SimpleCipher.new(key)
      var plaintext = "aaaaaaaaaa"
      Expect.value(cipher.decode(key)).toEqual(plaintext)
    }

    skip.test("Is reversible") {
      var key = "abdefghij"
      var cipher = SimpleCipher.new(key)
      var encoded = cipher.encode(key)
      var decoded = cipher.decode(encoded)
      Expect.value(decoded).toEqual(key)
    }

    skip.test("Can double shift encode") {
      var key = "iamapandabear"
      var cipher = SimpleCipher.new(key)
      var encoded = cipher.encode(key)
      var expected = "qayaeaagaciai"
      Expect.value(encoded).toEqual(expected)
    }

    skip.test("Can wrap on encode") {
      var cipher = SimpleCipher.new("abcdefghij")
      var encoded = cipher.encode("zzzzzzzzzz")
      Expect.value(encoded).toEqual("zabcdefghi")
    }

    skip.test("Can wrap on decode") {
      var cipher = SimpleCipher.new("abcdefghij")
      var decoded = cipher.decode("zabcdefghi")
      Expect.value(decoded).toEqual("zzzzzzzzzz")
    }

    skip.test("Can encode messages longer than the key") {
      var cipher = SimpleCipher.new("abc")
      var encoded = cipher.encode("iamapandabear")
      Expect.value(encoded).toEqual("iboaqcnecbfcr")
    }

    skip.test("Can decode messages longer than the key") {
      var cipher = SimpleCipher.new("abc")
      var decoded = cipher.decode("iboaqcnecbfcr")
      Expect.value(decoded).toEqual("iamapandabear")
    }
  }
}
