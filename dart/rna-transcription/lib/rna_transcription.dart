const dna2rna = {'G': 'C', 'C': 'G', 'T': 'A', 'A': 'U'};
final nucleotides = RegExp("[${dna2rna.keys.join('')}]");

class RnaTranscription {
  String toRna(String dna) =>
      dna.replaceAllMapped(nucleotides, (m) => dna2rna[m[0]!]!);
}
