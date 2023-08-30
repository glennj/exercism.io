# Pangram

Welcome to Pangram on Exercism's 8th Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

You work for a company that sells fonts through their website.
They'd like to show a different sentence each time someone views a font on their website.
To give a comprehensive sense of the font, the random sentences should use **all** the letters in the English alphabet.

They're running a competition to get suggestions for sentences that they can use.
You're in charge of checking the submissions to see if they are valid.

~~~~exercism/note
Pangram comes from Greek, παν γράμμα, pan gramma, which means "every letter".

The best known English pangram is:

> The quick brown fox jumps over the lazy dog.
~~~~

## Instructions

Your task is to figure out if a sentence is a pangram.

A pangram is a sentence using every letter of the alphabet at least once.
It is case insensitive, so it doesn't matter if a letter is lower-case (e.g. `k`) or upper-case (e.g. `K`).

For this exercise, a sentence is a pangram if it contains each of the 26 letters in the English alphabet.

## Running and testing your solutions
 
### From the command line
 
Simply type `8th test.8th`. It is assumed that the 8th binary is declared in the PATH environment variable.
 
### From a REPL
 
Start 8th loading test-words.8th and your solution file:
`8th -f test-words.8th -f pangram-tests.8th`
This will start a CLI session where you can run tests interactively by copying and pasting them in from pangram-tests.8th or by entering your own. 
 
### Editing the pangram-tests.8th file
 
This is encouraged at the beginning of your journey. Insert a back-slash space before all but the first one or two tests. Write your code to solve the first test or two. Then progressively enable more tests until you can pass all of them.

## Source

### Created by

- @axtens

### Based on

Wikipedia - https://en.wikipedia.org/wiki/Pangram