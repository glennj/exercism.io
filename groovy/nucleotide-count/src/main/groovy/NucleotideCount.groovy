class NucleotideCount {

    static count(strand) {
        def nucleotides = [ 
            'G': 0, 'C': 0, 'T': 0, 'A': 0
        ].withDefault { throw new ArithmeticException() }

        strand.each { nucleotides[it] += 1 }
        nucleotides
    }
}
