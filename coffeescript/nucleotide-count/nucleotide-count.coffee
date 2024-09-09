class NucleotideCount
  constructor: (dna) ->
    @nucleotideCounts = A: 0, C: 0, G: 0, T: 0
    for nucleotide in [dna...]
      throw new Error 'Invalid nucleotide strand' unless nucleotide of @nucleotideCounts
      @nucleotideCounts[nucleotide] += 1

  count: (nucleotide) -> 
    throw new Error 'Invalid nucleotide' unless nucleotide of @nucleotideCounts
    @nucleotideCounts[nucleotide]

module.exports = NucleotideCount
