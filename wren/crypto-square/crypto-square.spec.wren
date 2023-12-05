import "./crypto-square" for CryptoSquare
import "wren-testie/testie" for Testie, Expect

Testie.test("CryptoSquare.ciphertext()") { |do, skip|
  do.test("empty plaintext results in an empty ciphertext") {
    var phrase = ""
    var expect = ""
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }

  do.test("normalization results in empty plaintext") {
    var phrase = "... --- ..."
    var expect = ""
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }

  do.test("Lowercase") {
    var phrase = "A"
    var expect = "a"
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }

  do.test("Remove spaces") {
    var phrase = "  b "
    var expect = "b"
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }

  do.test("Remove punctuation") {
    var phrase = "@1,\%!"
    var expect = "1"
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }

  do.test("9 character plaintext results in 3 chunks of 3 characters") {
    var phrase = "This is fun!"
    var expect = "tsf hiu isn"
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }

  do.test("8 character plaintext results in 3 chunks, the last one with a trailing space") {
    var phrase = "Chill out."
    var expect = "clu hlt io "
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }

  do.test("54 character plaintext results in 7 chunks, the last two with trailing spaces") {
    var phrase = "If man was meant to stay on the ground, god would have given us roots."
    var expect = "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "
    Expect.value(CryptoSquare.ciphertext(phrase)).toEqual(expect)
  }
}
