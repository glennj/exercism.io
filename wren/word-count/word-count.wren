// This is an exercise where regular expression support
// would be welcome...

import "./byte" for Byte

class Words {
  static count(sentence) {
    var state = "starting"
    var word = ""
    var words = {}

    var addWord = Fn.new {
      word = word.trimEnd("'")
      if (!word.isEmpty) {
        if (words.containsKey(word)) {
          words[word] = words[word] + 1
        } else {
          words[word] = 1
        }
        word = ""
      }
    }

    Byte.bytes(sentence).each {|byte|
      if (state == "starting") {
        if (byte.isAlnum) {
          word = byte.downcase.toString
          state = "in_word"
        }
      } else if (state == "in_word") {
        if (byte.isAlnum || byte.isApostrophe) {
          word = word + byte.downcase.toString
        } else {
          addWord.call()
          state = "starting"
        }
      }
    }
    addWord.call()

    return words
  }
}
