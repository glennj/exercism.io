const proteins = {
  AUG: 'Methionine',   UUU: 'Phenylalanine',
  UUA: 'Leucine',      UUC: 'Phenylalanine',
  UUG: 'Leucine',      UGG: 'Tryptophan',   
  UCU: 'Serine',       UGC: 'Cysteine',   
  UCC: 'Serine',       UGU: 'Cysteine',
  UCA: 'Serine',
  UCG: 'Serine',       UAA: 'STOP',
  UAU: 'Tyrosine',     UGA: 'STOP',
  UAC: 'Tyrosine',     UAG: 'STOP',
};

export const translate = (rna = '') => {
  // Extract all 3-character chunks in the input string.
  // The `{1,3}` quantifier will pick up an straggling characters from
  // the end of the string.
  const codons = (rna).match(/.{1,3}/g) ?? [];
  const names = [];
  for (const codon of codons) {
    if (!(codon in proteins)) throw new Error('Invalid codon');
    if (proteins[codon] === 'STOP') break;
    names.push(proteins[codon]);
  }
  return names;
};
