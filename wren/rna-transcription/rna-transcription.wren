var COMPLEMENTS = { "G": "C", "C": "G", "T": "A", "A": "U" }

class DNA {
  static toRNA(strand) {
    return strand.reduce("") {|rna, nucl| 
      if (!COMPLEMENTS.containsKey(nucl)) {
        Fiber.abort("Invalid nucleotide %(nucl)")
      }
      return rna + COMPLEMENTS[nucl]
    }
  }
}
