module nucleotide_count;

import std.exception : enforce;
//import std.algorithm : canFind;
import std.string : lastIndexOfNeither;

class Counter {
    private static immutable auto nucleotides = "ACGT";
    private immutable string dna;
    
    this(string dna) {
        //foreach (n; dna) enforce(nucleotides.canFind(n), "Invalid nucleotide");
        enforce(dna.lastIndexOfNeither(nucleotides) == -1, "Invalid nucleotide");
        this.dna = dna;
    }

    pure const ulong[char] nucleotideCounts() {
        ulong[char] counts;
        foreach (n; nucleotides) counts[n] = 0;
        foreach (n; dna) counts[n]++;
        return counts;
    }
}


unittest
{
    import std.string;
    import std.array;
    import std.algorithm.sorting : sort;
    import std.algorithm.comparison : equal;

    // test associative array equality
    bool aaEqual(const ulong[char] lhs, const ulong[char] rhs)
    {
        auto lhs_pairs = lhs.byKeyValue.array;
        auto rhs_pairs = rhs.byKeyValue.array;
        lhs_pairs.sort!(q{a.key < b.key});
        rhs_pairs.sort!(q{a.key < b.key});

        return equal!("a.key == b.key && a.value == b.value")(lhs_pairs, rhs_pairs);
    }

    immutable int allTestsEnabled = 0;

    // Empty strand
    {
        const Counter dna = new Counter("");
        const ulong[char] expected = ['A' : 0, 'T' : 0, 'C' : 0, 'G' : 0];

        auto actual = dna.nucleotideCounts();

        assert(aaEqual(expected, actual));
    }

    static if (allTestsEnabled)
    {
        // Can count one nucleotide in single-character input
        {
            const Counter dna = new Counter("G");
            const ulong[char] expected = ['A' : 0, 'T' : 0, 'C' : 0, 'G' : 1];

            const auto actual = dna.nucleotideCounts();

            assert(aaEqual(expected, actual));
        }

        // Strand with repeated nucleotide
        {
            const Counter dna = new Counter("GGGGGGGG");
            const ulong[char] expected = ['A' : 0, 'T' : 0, 'C' : 0, 'G' : 8];

            const auto actual = dna.nucleotideCounts();

            assert(aaEqual(expected, actual));
        }

        // Strand with multiple nucleotides
        {
            const Counter dna = new Counter(
                    "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC");
            const ulong[char] expected = ['A' : 20, 'T' : 21, 'C' : 12, 'G' : 17];

            const auto actual = dna.nucleotideCounts();

            assert(aaEqual(expected, actual));
        }

        // Strand with invalid nucleotides
        {
            import std.exception : assertThrown;

            assertThrown(new Counter("AGXXACT"));
        }

    }

}
