use context essentials2020

include file("nucleotide-count.arr")

include string-dict

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun empty-strand():
  check "empty strand":
    input = ""
    expected = [string-dict: "A", 0, "C", 0, "G", 0, "T", 0]

    nucleotide-counts(input) is expected
  end
end

fun single-nucleotide-strand():
  check "can count one nucleotide in single-character input":
    input = "G"
    expected = [string-dict: "A", 0, "C", 0, "G", 1, "T", 0]

    nucleotide-counts(input) is expected
  end
end

fun repeated-nucleotide-strand():
  check "strand with repeated nucleotide":
    input = "GGGGGGG"
    expected = [string-dict: "A", 0, "C", 0, "G", 7, "T", 0]

    nucleotide-counts(input) is expected
  end
end

fun multiple-nucleotide-strand():
  check "strand with multiple nucleotides":
    input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    expected = [string-dict: "A", 20, "C", 12, "G", 17, "T", 21]

    nucleotide-counts(input) is expected
  end
end

fun invalid-nucleotide-strand():
  check "strand with invalid nucleotides":
    input = "AGXXACT"

    nucleotide-counts(input) raises "Invalid nucleotide in strand"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(empty-strand, true),
  test(single-nucleotide-strand, true),
  test(repeated-nucleotide-strand, true),
  test(multiple-nucleotide-strand, true),
  test(invalid-nucleotide-strand, true)
].each(lam(t): when t.active: t.run() end end)
