#!/usr/bin/env tclsh
# generated: 2026-07-24T19:22:59Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "protein-translation.tcl"


test protein-translation-1 "Empty RNA sequence results in no proteins" -body {
    proteins ""
} -returnCodes ok -result {}

skip protein-translation-2
test protein-translation-2 "Methionine RNA sequence" -body {
    proteins "AUG"
} -returnCodes ok -result {Methionine}

skip protein-translation-3
test protein-translation-3 "Phenylalanine RNA sequence 1" -body {
    proteins "UUU"
} -returnCodes ok -result {Phenylalanine}

skip protein-translation-4
test protein-translation-4 "Phenylalanine RNA sequence 2" -body {
    proteins "UUC"
} -returnCodes ok -result {Phenylalanine}

skip protein-translation-5
test protein-translation-5 "Leucine RNA sequence 1" -body {
    proteins "UUA"
} -returnCodes ok -result {Leucine}

skip protein-translation-6
test protein-translation-6 "Leucine RNA sequence 2" -body {
    proteins "UUG"
} -returnCodes ok -result {Leucine}

skip protein-translation-7
test protein-translation-7 "Serine RNA sequence 1" -body {
    proteins "UCU"
} -returnCodes ok -result {Serine}

skip protein-translation-8
test protein-translation-8 "Serine RNA sequence 2" -body {
    proteins "UCC"
} -returnCodes ok -result {Serine}

skip protein-translation-9
test protein-translation-9 "Serine RNA sequence 3" -body {
    proteins "UCA"
} -returnCodes ok -result {Serine}

skip protein-translation-10
test protein-translation-10 "Serine RNA sequence 4" -body {
    proteins "UCG"
} -returnCodes ok -result {Serine}

skip protein-translation-11
test protein-translation-11 "Tyrosine RNA sequence 1" -body {
    proteins "UAU"
} -returnCodes ok -result {Tyrosine}

skip protein-translation-12
test protein-translation-12 "Tyrosine RNA sequence 2" -body {
    proteins "UAC"
} -returnCodes ok -result {Tyrosine}

skip protein-translation-13
test protein-translation-13 "Cysteine RNA sequence 1" -body {
    proteins "UGU"
} -returnCodes ok -result {Cysteine}

skip protein-translation-14
test protein-translation-14 "Cysteine RNA sequence 2" -body {
    proteins "UGC"
} -returnCodes ok -result {Cysteine}

skip protein-translation-15
test protein-translation-15 "Tryptophan RNA sequence" -body {
    proteins "UGG"
} -returnCodes ok -result {Tryptophan}

skip protein-translation-16
test protein-translation-16 "STOP codon RNA sequence 1" -body {
    proteins "UAA"
} -returnCodes ok -result {}

skip protein-translation-17
test protein-translation-17 "STOP codon RNA sequence 2" -body {
    proteins "UAG"
} -returnCodes ok -result {}

skip protein-translation-18
test protein-translation-18 "STOP codon RNA sequence 3" -body {
    proteins "UGA"
} -returnCodes ok -result {}

skip protein-translation-19
test protein-translation-19 "Sequence of two protein codons translates into proteins" -body {
    proteins "UUUUUU"
} -returnCodes ok -result {Phenylalanine Phenylalanine}

skip protein-translation-20
test protein-translation-20 "Sequence of two different protein codons translates into proteins" -body {
    proteins "UUAUUG"
} -returnCodes ok -result {Leucine Leucine}

skip protein-translation-21
test protein-translation-21 "Translate RNA strand into correct protein list" -body {
    proteins "AUGUUUUGG"
} -returnCodes ok -result {Methionine Phenylalanine Tryptophan}

skip protein-translation-22
test protein-translation-22 "Translation stops if STOP codon at beginning of sequence" -body {
    proteins "UAGUGG"
} -returnCodes ok -result {}

skip protein-translation-23
test protein-translation-23 "Translation stops if STOP codon at end of two-codon sequence" -body {
    proteins "UGGUAG"
} -returnCodes ok -result {Tryptophan}

skip protein-translation-24
test protein-translation-24 "Translation stops if STOP codon at end of three-codon sequence" -body {
    proteins "AUGUUUUAA"
} -returnCodes ok -result {Methionine Phenylalanine}

skip protein-translation-25
test protein-translation-25 "Translation stops if STOP codon in middle of three-codon sequence" -body {
    proteins "UGGUAGUGG"
} -returnCodes ok -result {Tryptophan}

skip protein-translation-26
test protein-translation-26 "Translation stops if STOP codon in middle of six-codon sequence" -body {
    proteins "UGGUGUUAUUAAUGGUUU"
} -returnCodes ok -result {Tryptophan Cysteine Tyrosine}

skip protein-translation-27
test protein-translation-27 "Sequence of two non-STOP codons does not translate to a STOP codon" -body {
    proteins "AUGAUG"
} -returnCodes ok -result {Methionine Methionine}

skip protein-translation-28
test protein-translation-28 "Non-existing codon can't translate" -body {
    proteins "AAA"
} -returnCodes error -result "Invalid codon"

skip protein-translation-29
test protein-translation-29 "Unknown amino acids, not part of a codon, can't translate" -body {
    proteins "XYZ"
} -returnCodes error -result "Invalid codon"

skip protein-translation-30
test protein-translation-30 "Incomplete RNA sequence can't translate" -body {
    proteins "AUGU"
} -returnCodes error -result "Invalid codon"

skip protein-translation-31
test protein-translation-31 "Incomplete RNA sequence can translate if valid until a STOP codon" -body {
    proteins "UUCUUCUAAUGGU"
} -returnCodes ok -result {Phenylalanine Phenylalanine}


cleanupTests
