class Nucleotide {
  static count(strand) {
    var nc = { "A": 0, "C": 0, "G": 0, "T": 0}
    for (char in strand) {
      if (!nc.containsKey(char)) {
        Fiber.abort("Invalid nucleotide in strand")
      }
      nc[char] = nc[char] + 1
    }
    return nc
  }
}
