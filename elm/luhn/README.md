# Luhn

Welcome to Luhn on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

At the Global Verification Authority, you've just been entrusted with a critical assignment.
Across the city, from online purchases to secure logins, countless operations rely on the accuracy of numerical identifiers like credit card numbers, bank account numbers, transaction codes, and tracking IDs.
The Luhn algorithm is a simple checksum formula used to ensure these numbers are valid and error-free.

A batch of identifiers has just arrived on your desk.
All of them must pass the Luhn test to ensure they're legitimate.
If any fail, they'll be flagged as invalid, preventing errors or fraud, such as incorrect transactions or unauthorized access.

Can you ensure this is done right? The integrity of many services depends on you.

## Instructions

Determine whether a credit card number is valid according to the [Luhn formula][luhn].

The number will be provided as a string.

## Validating a number

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
4539 3195 0343 6467
↑ ↑  ↑ ↑  ↑ ↑  ↑ ↑  (double these)
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

## Source

### Created by

- @tuxagon

### Contributed to by

- @jiegillet
- @nathanielknight

### Based on

The Luhn Algorithm on Wikipedia - https://en.wikipedia.org/wiki/Luhn_algorithm