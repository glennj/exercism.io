include std/unittest.e
include std/map.e

include nucleotide-count.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Empty strand",
            {{'A', 0},{'C', 0},{'G', 0},{'T', 0}},
            pairs(counts(""), 1))
test_equal("Can count one nucleotide in single-character input strand",
            {{'A', 0},{'C', 0},{'G', 1},{'T', 0}},
            pairs(counts("G"), 1))
test_equal("Strand with repeated nucleotide",
            {{'A', 0},{'C', 0},{'G', 7},{'T', 0}},
            pairs(counts("GGGGGGG"), 1))
test_equal("Strand with multiple nucleotides",
            {{'A', 20},{'C', 12},{'G', 17},{'T', 21}},
            pairs(counts("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"), 1))
test_false("Strand with invalid nucleotides",
            counts("AGXXACT"))

test_report()