EnglishAlphabetSize = 26

class Pangram
  @isPangram: (sentence) ->
    letters = new Set [sentence.replace(/\P{Letter}/ug, '').toLowerCase()...]
    letters.size is EnglishAlphabetSize

module.exports = Pangram
