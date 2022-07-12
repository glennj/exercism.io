import "./byte" for Byte
import "./math" for Math

class AffineCipher {
  static initialize() {
    __alphabet = Byte.downcase(Byte.ALPHABET)
    __m = __alphabet.count
  }

  static encode(text, opts) {
    return this.new(text, opts["a"], opts["b"]).encoded
  }
  
  static decode(text, opts) {
    return this.new(text, opts["a"], opts["b"]).decoded
  }


  construct new(text, a, b) {
    validate(a)
    _text = Byte.toString(
      Byte.bytes(text)
          .where {|b| b.isAlnum}
          .map {|b| b.downcase}
    )
    _a = a
    _b = b
  }

  validate(a) {
    if (Math.gcd(a, __m) != 1) {
      Fiber.abort("a and m must be coprime.")
    }
  }

  encoded {
    var func = Fn.new {|x| (_a * x + _b) % __m}
    return chunksOf(5, code_(func))
  }

  decoded {
    var aInv = Math.mmi(_a, __m)
    var func = Fn.new {|y| (aInv * (y - _b)) % __m}
    return code_(func)
  }

  code_(func) {
    var mapping = (0...__m).reduce({}) {|m, i| 
      m[__alphabet[i]] = __alphabet[func.call(i)]
      return m
    }
    return _text
      .map {|c| mapping.containsKey(c) ? mapping[c] : c}
      .join()
  }

  chunksOf(size, str) {
    var chunks = []
    while (!str.isEmpty) {
      var len = size.min(str.count)
      chunks.add(str.take(len).join())
      str = str.skip(len)
    }
    return chunks.join(" ")
  }
}

AffineCipher.initialize()
