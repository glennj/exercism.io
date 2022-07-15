import "./rna-transcription" for DNA
import "wren-testie/testie" for Testie, Expect

Testie.test("RNA Transcription") { |do, skip|
  do.test("empty rna sequence") {
    Expect.value(DNA.toRNA("")).toEqual("")
  }

  skip.test("transcribes cytosine to guanine") {
    Expect.value(DNA.toRNA("C")).toEqual("G")
  }

  skip.test("transcribes guanine to cytosine") {
    Expect.value(DNA.toRNA("G")).toEqual("C")
  }

  skip.test("transcribes thymine to adenine") {
    Expect.value(DNA.toRNA("T")).toEqual("A")
  }

  skip.test("transcribes adenine to uracil") {
    Expect.value(DNA.toRNA("A")).toEqual("U")
  }

  skip.test("transcribes all dna nucleotides to their rna complements") {
    Expect.value(DNA.toRNA("ACGTGGTCTTAA")).toEqual("UGCACCAGAAUU")
  }
}
