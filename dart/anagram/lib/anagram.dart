class Anagram {
  List<String> findAnagrams(String word, List<String> candidates) {
    var wordKey = (String word) {
      var rs = word.runes.toList();
      rs.sort();
      return rs;
    };

    var wordLower = word.toLowerCase();
    var key = wordKey(wordLower);

    var isAnagram = (candidate) {
      var lower = candidate.toLowerCase();
      return wordLower != lower && key == wordKey(lower);
    };

    return candidates.where(isAnagram).toList();
  }
}
