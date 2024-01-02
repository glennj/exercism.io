use context essentials2020

include file("protein-translation.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun no-protein():
  check "Empty RNA sequence results in no proteins":
    proteins("") is [list: ]
  end
end

fun methionine():
  check "Methionine RNA sequence":
    proteins("AUG") is [list: "Methionine"]
  end
end

fun phenylalanine-one():
  check "Phenylalanine RNA sequence 1":
    proteins("UUU") is [list: "Phenylalanine"]
  end
end

fun phenylalanine-two():
  check "Phenylalanine RNA sequence 2":
    proteins("UUC") is [list: "Phenylalanine"]
  end
end

fun leucine-one():
  check "Leucine RNA sequence 1":
    proteins("UUA") is [list: "Leucine"]
  end
end

fun leucine-two():
  check "Leucine RNA sequence 2":
    proteins("UUG") is [list: "Leucine"]
  end
end

fun serine-one():
  check "Serine RNA sequence 1":
    proteins("UCU") is [list: "Serine"]
  end
end

fun serine-two():
  check "Serine RNA sequence 2":
    proteins("UCC") is [list: "Serine"]
  end
end

fun serine-three():
  check "Serine RNA sequence 3":
    proteins("UCA") is [list: "Serine"]
  end
end

fun serine-four():
  check "Serine RNA sequence 4":
    proteins("UCG") is [list: "Serine"]
  end
end

fun tyrosine-one():
  check "Tyrosine RNA sequence 1":
    proteins("UAU") is [list: "Tyrosine"]
  end
end

fun tyrosine-two():
  check "Tyrosine RNA sequence 2":
    proteins("UAC") is [list: "Tyrosine"]
  end
end

fun cysteine-one():
  check "Cysteine RNA sequence 1":
    proteins("UGU") is [list: "Cysteine"]
  end
end

fun cysteine-two():
  check "Cysteine RNA sequence 2":
    proteins("UGC") is [list: "Cysteine"]
  end
end

fun tryptophan():
  check "Tryptophan RNA sequence":
    proteins("UGG") is [list: "Tryptophan"]
  end
end

fun stop-one():
  check "STOP codon RNA sequence 1":
    proteins("UAA") is [list: ]
  end
end

fun stop-two():
  check "STOP codon RNA sequence 2":
    proteins("UAG") is [list: ]
  end
end

fun stop-three():
  check "STOP codon RNA sequence 3":
    proteins("UGA") is [list: ]
  end
end

fun two-identical-proteins():
  check "Sequence of two protein codons translates into proteins":
    proteins("UUUUUU") is [list: "Phenylalanine", "Phenylalanine"]
  end
end

fun two-different-proteins():
  check "Sequence of two different protein codons translates into proteins":
    proteins("UUAUUG") is [list: "Leucine", "Leucine"]
  end
end

fun three-different-proteins():
  check "Translate RNA strand into correct protein list":
    proteins("AUGUUUUGG") is [list: "Methionine", "Phenylalanine", "Tryptophan"]
  end
end

fun stop-at-beginning():
  check "Translation stops if STOP codon at beginning of sequence":
    proteins("UAGUGG") is [list: ]
  end
end

fun stop-at-end-of-two-codons():
  check "Translation stops if STOP codon at end of two-codon sequence":
    proteins("UGGUAG") is [list: "Tryptophan"]
  end
end

fun stop-at-end-of-three-codons():
  check "Translation stops if STOP codon at end of three-codon sequence":
    proteins("AUGUUUUAA") is [list: "Methionine", "Phenylalanine"]
  end
end

fun stop-in-middle-of-three-codons():
  check "Translation stops if STOP codon in middle of three-codon sequence":
    proteins("UGGUAGUGG") is [list: "Tryptophan"]
  end
end

fun stop-in-middle-of-six-codons():
  check "Translation stops if STOP codon in middle of six-codon sequence":
    proteins("UGGUGUUAUUAAUGGUUU") is [list: "Tryptophan", "Cysteine", "Tyrosine"]
  end
end

fun non-existent-codon():
  check "Non-existing codon can't translate":
    proteins("AAA") raises "Invalid codon"
  end
end

fun unknown-codon():
  check "Unknown amino acids, not part of a codon, can't translate":
    proteins("XYZ") raises "Invalid codon"
  end
end

fun incomplete-sequence():
  check "Incomplete RNA sequence can't translate":
    proteins("AUGU") raises "Invalid codon"
  end
end

fun incomplete-sequence-with-stop():
  check "Incomplete RNA sequence can translate if valid until a STOP codon":
    proteins("UUCUUCUAAUGGU") is [list: "Phenylalanine", "Phenylalanine"]
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(no-protein, true),
  test(methionine, true),
  test(phenylalanine-one, true),
  test(phenylalanine-two, true),
  test(leucine-one, true),
  test(leucine-two, true),
  test(serine-one, true),
  test(serine-two, true),
  test(serine-three, true),
  test(serine-four, true),
  test(tyrosine-one, true),
  test(tyrosine-two, true),
  test(cysteine-one, true),
  test(cysteine-two, true),
  test(tryptophan, true),
  test(stop-one, true),
  test(stop-two, true),
  test(stop-three, true),
  test(two-identical-proteins, true),
  test(two-different-proteins, true),
  test(three-different-proteins, true),
  test(stop-at-beginning, true),
  test(stop-at-end-of-two-codons, true),
  test(stop-at-end-of-three-codons, true),
  test(stop-in-middle-of-three-codons, true),
  test(stop-in-middle-of-six-codons, true),
  test(non-existent-codon, true),
  test(unknown-codon, true),
  test(incomplete-sequence, true),
  test(incomplete-sequence-with-stop, true)
].each(lam(t): when t.active: t.run() end end)
