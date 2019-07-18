class Hamming {

    def distance(strand1, strand2) {
        if (strand1.size() != strand2.size())
            throw new ArithmeticException()

        /* first take
        [ strand1.toList(), strand2.toList() ]
            .transpose()
            .findAll {c1, c2 -> c1 != c2}
            .size()
        */

        [strand1, strand2]
            *.toList()
            .transpose()
            .count {c1, c2 -> c1 != c2}
    }
}
