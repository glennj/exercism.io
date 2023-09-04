# Luhn Credit Card Number Checker

Welcome to Luhn Credit Card Number Checker on Exercism's 8th Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a number determine whether or not it is valid per the Luhn formula.

The [Luhn algorithm][luhn] is a simple checksum formula used to validate a variety of identification numbers, such as credit card numbers and Canadian Social Insurance Numbers.

The task is to check if a given string is valid.

## Validating a Number

Strings of length 1 or less are not valid.
Spaces are allowed in the input, but they should be stripped before checking.
All other non-digit characters are disallowed.

### Example 1: valid credit card number

```text
4539 3195 0343 6467
```

The first step of the Luhn algorithm is to double every second digit, starting from the right.
We will be doubling

```text
4_3_ 3_9_ 0_4_ 6_6_
```

If doubling the number results in a number greater than 9 then subtract 9 from the product.
The results of our doubling:

```text
8569 6195 0383 3437
```

Then sum all of the digits:

```text
8+5+6+9+6+1+9+5+0+3+8+3+3+4+3+7 = 80
```

If the sum is evenly divisible by 10, then the number is valid.
This number is valid!

### Example 2: invalid credit card number

```text
8273 1232 7352 0569
```

Double the second digits, starting from the right

```text
7253 2262 5312 0539
```

Sum the digits

```text
7+2+5+3+2+2+6+2+5+3+1+2+0+5+3+9 = 57
```

57 is not evenly divisible by 10, so this number is not valid.

[luhn]: https://en.wikipedia.org/wiki/Luhn_algorithm

## Running and testing your solutions
 
### From the command line
 
Simply type `8th test.8th`. It is assumed that the 8th binary is declared in the PATH environment variable.
 
### From a REPL

Start 8th loading test-words.8th: `8th -f test-words.8th`.
This will start a CLI session where, once you’ve written some code in your solution file, you can load it with `"luhn.8th" f:include`
and you can run the tests with `"luhn-tests.8th" f:include` or you can copy and paste a single test from that file or enter your own. 
 
### Editing the luhn-tests.8th file
 
This is encouraged at the beginning of your journey. Insert a back-slash space before all but the first one or two tests. Write your code to solve the first test or two. Then progressively enable more tests until you can pass all of them.

## Source

### Created by

- @rosettacode

### Based on

The Luhn Algorithm on Wikipedia - https://en.wikipedia.org/wiki/Luhn_algorithm