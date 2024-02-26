/* This solution is somewhat inefficient: 
 * - it does not return as soon as a stop is seen
 * - it does not error as soon as an invalid codon is seen.
 */

class ProteinTranslation {
  static const stop = 'stop';
  static const invalid = 'invalid';

  List<String> translate(String rna) {
    var proteins = RegExp('.{1,3}')
        .allMatches(rna)
        .map((m) => _codon2protein(m[0]!))
        .takeWhile((protein) => protein != stop);

    if (proteins.contains(invalid)) {
      throw ArgumentError('invalid codon');
    }

    return proteins.toList();
  }

  String _codon2protein(String codon) {
    switch (codon) {
      case 'AUG':
        return 'Methionine';
      case 'UUU':
      case 'UUC':
        return 'Phenylalanine';
      case 'UUA':
      case 'UUG':
        return 'Leucine';
      case 'UCU':
      case 'UCC':
      case 'UCA':
      case 'UCG':
        return 'Serine';
      case 'UAU':
      case 'UAC':
        return 'Tyrosine';
      case 'UGU':
      case 'UGC':
        return 'Cysteine';
      case 'UGG':
        return 'Tryptophan';
      case 'UAA':
      case 'UAG':
      case 'UGA':
        return stop;
      default:
        return invalid;
    }
  }
}
