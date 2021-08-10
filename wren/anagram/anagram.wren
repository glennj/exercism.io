import "./byte" for Byte

class Anagram {
  static find(word, list) {
    var anagramKey = Fn.new {|w|
      var bytes = Byte.bytes(w).map {|b| b.upcase}.toList
      bytes.sort()
      return Byte.toString(bytes)
    }

    var up = Byte.upcase(word)
    var k = anagramKey.call(word)
    return list
      .where {|w| Byte.upcase(w) != up && anagramKey.call(w) == k}
      .toList
  }
}
