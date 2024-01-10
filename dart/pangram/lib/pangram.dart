// Hmm, does this depend on locale?
bool isLower(String char) =>
    char.compareTo('a') >= 0 && char.compareTo('z') <= 0;

const alphabetSize = 26;

class Pangram {
  bool isPangram(String word) =>
      alphabetSize == word.toLowerCase().split('').where(isLower).toSet().length;
}
