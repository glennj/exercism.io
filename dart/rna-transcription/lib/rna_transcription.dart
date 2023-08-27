class RnaTranscription {
  final dna2rna = {'G': 'C', 'C': 'G', 'T': 'A', 'A': 'U'};

  String toRna(String dna) =>
    dna.split('')
       .map((nucleotide) => dna2rna[nucleotide])
       .join('');
}
