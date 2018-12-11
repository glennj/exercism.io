const proteins = {
  AUG: 'Methionine',
  UUU: 'Phenylalanine',
  UUC: 'Phenylalanine',
  UUA: 'Leucine',
  UUG: 'Leucine',
  UCU: 'Serine',
  UCC: 'Serine',
  UCA: 'Serine',
  UCG: 'Serine',
  UAU: 'Tyrosine',
  UAC: 'Tyrosine',
  UGU: 'Cysteine',
  UGC: 'Cysteine',
  UGG: 'Tryptophan',
  UAA: 'STOP',
  UAG: 'STOP',
  UGA: 'STOP',
};

const translate = (rna = '') => {
  if (rna.length % 3 !== 0) throw new Error('Invalid codon');
  const codons = (rna).match(/.../g) || [];
  const names = [];
  for (let i = 0; i < codons.length; i += 1) {
    if (!(codons[i] in proteins)) throw new Error('Invalid codon');
    if (proteins[codons[i]] === 'STOP') break;
    names.push(proteins[codons[i]]);
  }
  return names;
};

module.exports = translate;

/* community
 *
 * interesting way to end for loop:
      for (var token, i = 0; token = tokens[i]; i ++) {
 */
