const dna2rna: { [key: string]: string } = {
  G: 'C',
  C: 'G',
  T: 'A',
  A: 'U'
}

export function toRna( dna: string ): string {
  return [...dna].reduce((rna, nucleotide) => {
    if (nucleotide in dna2rna)
      return rna + dna2rna[nucleotide]
    else
      throw new Error('Invalid input DNA.')
  }, '')
}
