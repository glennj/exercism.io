class Transcriptor {
    static dna2rna: { [key: string]: string } = {
        G: 'C',
        C: 'G',
        T: 'A',
        A: 'U'
    }

    static badDnaRegex = new RegExp( '[^GCTA]' )

    toRna( dna: string ): string {
        if (Transcriptor.badDnaRegex.test(dna)) {
            throw new Error('Invalid input DNA.')
        }
        return Array.from(dna).reduce(
            (rna, nucleotide) => {
                return rna + Transcriptor.dna2rna[nucleotide]
            }, ''
        )
    }
}

export default Transcriptor
