# Roman Numerals

Welcome to Roman Numerals on Exercism's 8th Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Write a function to convert from normal numbers to Roman Numerals.

The Romans were a clever bunch.
They conquered most of Europe and ruled it for hundreds of years.
They invented concrete and straight roads and even bikinis.
One thing they never discovered though was the number zero.
This made writing and dating extensive histories of their exploits slightly more challenging, but the system of numbers they came up with is still in use today.
For example the BBC uses Roman numerals to date their programs.

The Romans wrote numbers using letters - I, V, X, L, C, D, M.
(notice these letters have lots of straight lines and are hence easy to hack into stone tablets).

```text
 1  => I
10  => X
 7  => VII
```

The maximum number supported by this notation is 3,999.
(The Romans themselves didn't tend to go any higher)

Wikipedia says: Modern Roman numerals ... are written by expressing each digit separately starting with the left most digit and skipping any digit with a value of zero.

To see this in practice, consider the example of 1990.

In Roman numerals 1990 is MCMXC:

1000=M
900=CM
90=XC

2008 is written as MMVIII:

2000=MM
8=VIII

Learn more about [Roman numerals on Wikipedia][roman-numerals].

[roman-numerals]: https://wiki.imperivm-romanvm.com/wiki/Roman_Numerals

## Running and testing your solutions
 
### From the command line
 
Simply type `8th test.8th`. It is assumed that the 8th binary is declared in the PATH environment variable.
 
### From a REPL
 
Start 8th loading test-words.8th and your solution file:
`8th -f test-words.8th -f roman-numerals-tests.8th`
This will start a CLI session where you can run tests interactively by copying and pasting them in from roman-numerals-tests.8th or by entering your own. 
 
### Editing the roman-numerals-tests.8th file
 
This is encouraged at the beginning of your journey. Insert a back-slash space before all but the first one or two tests. Write your code to solve the first test or two. Then progressively enable more tests until you can pass all of them.

## Source

### Created by

- @jalih

### Contributed to by

- @axtens

### Based on

The Roman Numeral Kata - https://codingdojo.org/kata/RomanNumerals/