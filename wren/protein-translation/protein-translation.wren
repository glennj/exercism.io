var Codon2Protein = {
  "AUG": "Methionine",    "UCU": "Serine",
  "UUU": "Phenylalanine", "UCC": "Serine",
  "UUC": "Phenylalanine", "UCA": "Serine",
  "UUA": "Leucine",       "UCG": "Serine",
  "UUG": "Leucine",       "UAU": "Tyrosine",
  "UGG": "Tryptophan",    "UAC": "Tyrosine",
  "UAA": "STOP",          "UGU": "Cysteine",
  "UAG": "STOP",          "UGC": "Cysteine",
  "UGA": "STOP",
}

class Tools {
  static translate() {translate("")}

  static translate(strand) {
    var proteins = []
    while (!strand.isEmpty) {
      var codon = strand.take(3).join()
      if (!Codon2Protein.containsKey(codon)) {
        Fiber.abort("Invalid codon")
      }
      if (Codon2Protein[codon] == "STOP") {
        break
      }
      proteins.add(Codon2Protein[codon])
      strand = strand.skip(3).join()
    }
    return proteins
  }
}
