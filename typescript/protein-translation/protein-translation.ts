const codon2protein: {[key: string]: string} = {
  AUG: 'Methionine',
  UUU: 'Phenylalanine', UUC: 'Phenylalanine',
  UUA: 'Leucine', UUG: 'Leucine',
  UCU: 'Serine', UCC: 'Serine', UCA: 'Serine', UCG: 'Serine',
  UAU: 'Tyrosine', UAC: 'Tyrosine',
  UGU: 'Cysteine', UGC: 'Cysteine',
  UGG: 'Tryptophan',
  UAA: 'STOP', UAG: 'STOP', UGA: 'STOP',
}

export function translate( rna: string ): string[] {
  const codons = rna.match(/(.{1,3})/g) ?? []
  const proteins: string[] = []
  for (const codon of codons) {
    if (!(codon in codon2protein)) {
      throw new Error('invalid rna')
    }
    if (codon2protein[codon] === 'STOP') {
      break
    }
    proteins.push( codon2protein[codon] )
  }
  return proteins
}
