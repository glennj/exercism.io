# Reverse String

Welcome to Reverse String on Exercism's Perl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

Reversing strings (reading them from right to left, rather than from left to right) is a surprisingly common task in programming.

For example, in bioinformatics, reversing the sequence of DNA or RNA strings is often important for various analyses, such as finding complementary strands or identifying palindromic sequences that have biological significance.

## Instructions

Your task is to reverse a given string.

Some examples:

- Turn `"stressed"` into `"desserts"`.
- Turn `"strops"` into `"sports"`.
- Turn `"racecar"` into `"racecar"`.

## Unicode and grapheme clusters

This exercise includes tests with Unicode strings that contain grapheme clusters.
Perl's  builtin string processing functions are unsufficient to handle these strings.
Here are a couple of resources to help you:

* [Treat Unicode strings as grapheme clusters][foy]
* [Extract by Grapheme Instead of Codepoint (regex)][christiansen]

[foy]: https://www.effectiveperlprogramming.com/2011/06/treat-unicode-strings-as-grapheme-clusters/
[christiansen]: https://www.perl.com/pub/2012/05/perlunicookbook-extract-by-grapheme-instead-of-codepoint-regex.html/

## Source

### Created by

- @glennj

### Based on

Introductory challenge to reverse an input string - https://medium.freecodecamp.org/how-to-reverse-a-string-in-javascript-in-3-different-ways-75e4763c68cb