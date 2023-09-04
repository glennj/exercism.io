# Acronym

Welcome to Acronym on Exercism's 8th Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Convert a phrase to its acronym.

Techies love their TLA (Three Letter Acronyms)!

Help generate some jargon by writing a program that converts a long name like Portable Network Graphics to its acronym (PNG).

Punctuation is handled as follows: hyphens are word separators (like whitespace); all other punctuation can be removed from the input.

For example:

|Input|Output|
|-|-|
|As Soon As Possible|ASAP|
|Liquid-crystal display|LCD|
|Thank George It's Friday!|TGIF|

## Running and testing your solutions
 
### From the command line
 
Simply type `8th test.8th`. It is assumed that the 8th binary is declared in the PATH environment variable.
 
### From a REPL

Start 8th loading test-words.8th: `8th -f test-words.8th`.
This will start a CLI session where, once you’ve written some code in your solution file, you can load it with `"acronym.8th" f:include`
and you can run the tests with `"acronym-tests.8th" f:include` or you can copy and paste a single test from that file or enter your own. 
 
### Editing the acronym-tests.8th file
 
This is encouraged at the beginning of your journey. Insert a back-slash space before all but the first one or two tests. Write your code to solve the first test or two. Then progressively enable more tests until you can pass all of them.

## Source

### Created by

- @jalih

### Based on

Julien Vanier - https://github.com/monkbroc