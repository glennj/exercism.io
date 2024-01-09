bool isASCIILetter(String char) {
  // Hmm, does this depend on locale?
  return char.compareTo('a') >= 0
      && char.compareTo('z') <= 0;
}

const ASCII_ALPHABET_SIZE = 26;

class Pangram {
  bool isPangram(String word) {
    var count = word.toLowerCase()
                    .split('')
                    .where(isASCIILetter)
                    .toSet()
                    .length;
    return count == ASCII_ALPHABET_SIZE;
  }
}
