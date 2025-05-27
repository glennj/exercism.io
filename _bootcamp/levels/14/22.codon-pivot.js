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

export function translateRna(rna) {
  const aminoAcids = [];
  for (let idx = 0; idx < rna.length; idx += 3) {
    const aminoAcid = proteins[rna.slice(idx, idx + 3)];
    if (aminoAcid === "STOP") break;
    aminoAcids.push(aminoAcid);    
  }
  return aminoAcids;
}
