proc to-rna {dna} {
    return [string map {G C  C G  T A  A U} $dna]
}
