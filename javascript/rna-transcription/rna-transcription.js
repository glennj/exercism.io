const dna2rna = {
  G: 'C',
  C: 'G',
  T: 'A',
  A: 'U',
};

export const toRna = (dna) => {
  let rna = '';
  [...dna.toUpperCase()].forEach((nucleotide) => {
    if (!(nucleotide in dna2rna)) {
      throw new Error('Invalid input DNA.');
    }
    rna = rna.concat(dna2rna[nucleotide]);
  });
  return rna;
};
