include std/unittest.e

include rna-transcription.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Empty RNA sequence", "", to_rna(""))
test_equal("RNA complement of cytosine is guanine", "G", to_rna("C"))
test_equal("RNA complement of guanine is cytosine", "C", to_rna("G"))
test_equal("RNA complement of thymine is adenine", "A", to_rna("T"))
test_equal("RNA complement of adenine is uracil", "U", to_rna("A"))
test_equal("RNA complement", "UGCACCAGAAUU", to_rna("ACGTGGTCTTAA"))

test_report()
