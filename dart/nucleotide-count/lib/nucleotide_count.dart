class InvalidNucleotideException implements Exception {}

class NucleotideCount {
  Map<String, int> count(String strand) {
    var counter = {"A": 0, "C": 0, "G": 0, "T": 0};
    for (var c in strand.split('')) {
      if (!counter.containsKey(c)) throw new InvalidNucleotideException();
      counter.update(c, (n) => n + 1);
    }
    return counter;
  }
}
