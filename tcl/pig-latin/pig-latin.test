#!/usr/bin/env tclsh
# generated: 2026-07-22T21:52:59Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "pig-latin.tcl"


test pig-latin-1 "ay is added to words that start with vowels: word beginning with a" -body {
    translate "apple"
} -returnCodes ok -result "appleay"

skip pig-latin-2
test pig-latin-2 "ay is added to words that start with vowels: word beginning with e" -body {
    translate "ear"
} -returnCodes ok -result "earay"

skip pig-latin-3
test pig-latin-3 "ay is added to words that start with vowels: word beginning with i" -body {
    translate "igloo"
} -returnCodes ok -result "iglooay"

skip pig-latin-4
test pig-latin-4 "ay is added to words that start with vowels: word beginning with o" -body {
    translate "object"
} -returnCodes ok -result "objectay"

skip pig-latin-5
test pig-latin-5 "ay is added to words that start with vowels: word beginning with u" -body {
    translate "under"
} -returnCodes ok -result "underay"

skip pig-latin-6
test pig-latin-6 "ay is added to words that start with vowels: word beginning with a vowel and followed by a qu" -body {
    translate "equal"
} -returnCodes ok -result "equalay"

skip pig-latin-7
test pig-latin-7 "first letter and ay are moved to the end of words that start with consonants: word beginning with p" -body {
    translate "pig"
} -returnCodes ok -result "igpay"

skip pig-latin-8
test pig-latin-8 "first letter and ay are moved to the end of words that start with consonants: word beginning with k" -body {
    translate "koala"
} -returnCodes ok -result "oalakay"

skip pig-latin-9
test pig-latin-9 "first letter and ay are moved to the end of words that start with consonants: word beginning with x" -body {
    translate "xenon"
} -returnCodes ok -result "enonxay"

skip pig-latin-10
test pig-latin-10 "first letter and ay are moved to the end of words that start with consonants: word beginning with q without a following u" -body {
    translate "qat"
} -returnCodes ok -result "atqay"

skip pig-latin-11
test pig-latin-11 "first letter and ay are moved to the end of words that start with consonants: word beginning with consonant and vowel containing qu" -body {
    translate "liquid"
} -returnCodes ok -result "iquidlay"

skip pig-latin-12
test pig-latin-12 "some letter clusters are treated like a single consonant: word beginning with ch" -body {
    translate "chair"
} -returnCodes ok -result "airchay"

skip pig-latin-13
test pig-latin-13 "some letter clusters are treated like a single consonant: word beginning with qu" -body {
    translate "queen"
} -returnCodes ok -result "eenquay"

skip pig-latin-14
test pig-latin-14 "some letter clusters are treated like a single consonant: word beginning with qu and a preceding consonant" -body {
    translate "square"
} -returnCodes ok -result "aresquay"

skip pig-latin-15
test pig-latin-15 "some letter clusters are treated like a single consonant: word beginning with th" -body {
    translate "therapy"
} -returnCodes ok -result "erapythay"

skip pig-latin-16
test pig-latin-16 "some letter clusters are treated like a single consonant: word beginning with thr" -body {
    translate "thrush"
} -returnCodes ok -result "ushthray"

skip pig-latin-17
test pig-latin-17 "some letter clusters are treated like a single consonant: word beginning with sch" -body {
    translate "school"
} -returnCodes ok -result "oolschay"

skip pig-latin-18
test pig-latin-18 "some letter clusters are treated like a single vowel: word beginning with yt" -body {
    translate "yttria"
} -returnCodes ok -result "yttriaay"

skip pig-latin-19
test pig-latin-19 "some letter clusters are treated like a single vowel: word beginning with xr" -body {
    translate "xray"
} -returnCodes ok -result "xrayay"

skip pig-latin-20
test pig-latin-20 "position of y in a word determines if it is a consonant or a vowel: y is treated like a consonant at the beginning of a word" -body {
    translate "yellow"
} -returnCodes ok -result "ellowyay"

skip pig-latin-21
test pig-latin-21 "position of y in a word determines if it is a consonant or a vowel: y is treated like a vowel at the end of a consonant cluster" -body {
    translate "rhythm"
} -returnCodes ok -result "ythmrhay"

skip pig-latin-22
test pig-latin-22 "position of y in a word determines if it is a consonant or a vowel: y as second letter in two letter word" -body {
    translate "my"
} -returnCodes ok -result "ymay"

skip pig-latin-23
test pig-latin-23 "phrases are translated: a whole phrase" -body {
    translate "quick fast run"
} -returnCodes ok -result "ickquay astfay unray"


cleanupTests
