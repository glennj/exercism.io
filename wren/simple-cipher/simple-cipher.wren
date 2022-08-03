import "random" for Random

var Alphabet = "abcdefghijklmnopqrstuvwxyz"

class Math {
  static floorMod(num, div) {(num % div +div) % div}
}

class SimpleCipher {
  construct new(key) {
    _key = key
  }
  
  static new() {
    var rand = Random.new()
    var key = (1..100).map {rand.sample(Alphabet)}.join("")
    return new(key)
  }  

  key {_key}

  encode(plaintext)  {cipher_(plaintext)  {|a, b| Math.floorMod(a + b, 26)}}
  decode(ciphertext) {cipher_(ciphertext) {|a, b| Math.floorMod(a - b, 26)}}
  
  cipher_(text, cipherFn) {
    var len = text.count
    var key = extendKey_(len)
    return (0...len).reduce("") {|encoded, i|
      var txtIdx = Alphabet.indexOf(text[i])
      var keyIdx = Alphabet.indexOf(key[i])
      var newIdx = cipherFn.call(txtIdx, keyIdx)
      return encoded + Alphabet[newIdx]
    }
  }

  /* return the key, duplicated as many times as needed,
   * so that it is the same length as the input text
   */
  extendKey_(len) {
    var keyLen = _key.count
    var key = _key
    var kl = keyLen
    while (kl < len) {
      key = key + _key
      kl = kl + keyLen
    }
    return key[0...len]
  }
}
