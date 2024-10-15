# Nucleotide Count

Welcome to Nucleotide Count on Exercism's Dart Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Each of us inherits from our biological parents a set of chemical instructions known as DNA that influence how our bodies are constructed.
All known life depends on DNA!

> Note: You do not need to understand anything about nucleotides or DNA to complete this exercise.

DNA is a long chain of other chemicals and the most important are the four nucleotides, adenine, cytosine, guanine and thymine.
A single DNA chain can contain billions of these four nucleotides and the order in which they occur is important!
We call the order of these nucleotides in a bit of DNA a "DNA sequence".

We represent a DNA sequence as an ordered collection of these four nucleotides and a common way to do that is with a string of characters such as "ATTACG" for a DNA sequence of 6 nucleotides.
'A' for adenine, 'C' for cytosine, 'G' for guanine, and 'T' for thymine.

Given a string representing a DNA sequence, count how many of each nucleotide is present.
If the string contains characters that aren't A, C, G, or T then it is invalid and you should signal an error.

For example:

```text
"GATTACA" -> 'A': 3, 'C': 1, 'G': 1, 'T': 2
"INVALID" -> error
```

There are times when you need to handle unexpected issues that arise during code execution.
In Dart, you can use exceptions to handle these situations.

To throw an exception in Dart, use the `throw` keyword followed by an instance of the specific exception you want to raise.
For example, `throw Exception()` throws an instance of the base Exception type.

In this exercise, you need to define a custom exception called `InvalidNucleotideException`.
This exception should be raised when an invalid nucleotide is encountered.
The test suite only checks that an instance of `InvalidNucleotideException` is passed, so you have the freedom to implement it as you see fit as long as it is correctly thrown.

For more information on exceptions in Dart, you can refer to the [exceptions documentation](https://dart.dev/language/error-handling).
Custom exceptions can be created by [implementing the Exception interface](https://dart.dev/language#interfaces-and-abstract-classes).

## Source

### Created by

- @BNAndras

### Based on

The Calculating DNA Nucleotides_problem at Rosalind - https://rosalind.info/problems/dna/