import "wren-testie/testie" for Testie, Expect
import "./nucleotide-count" for Nucleotide

Testie.test("count nucleotides") { |do, skip|
  do.test("empty strand") {
    Expect.value(Nucleotide.count("")).toEqual({ "A": 0, "C": 0, "G": 0, "T": 0})
  }

  do.test("can count one nucleotide in single-character input") {
    Expect.value(Nucleotide.count("G")).toEqual({ "A": 0, "C": 0, "G": 1, "T": 0})
  }

  do.test("strand with repeated nucleotide") {
    Expect.value(Nucleotide.count("GGGGGGG")).toEqual({ "A": 0, "C": 0, "G": 7, "T": 0})
  }

  do.test("strand with multiple nucleotides") {
    Expect.value(
      Nucleotide.count(
        "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
      )
    ).toEqual({ "A": 20, "C": 12, "G": 17, "T": 21})
  }

  do.test("strand with invalid nucleotides") {
    Expect.that {
      Nucleotide.count("AGXXACT")
    }.abortsWith("Invalid nucleotide in strand")
  }
}
