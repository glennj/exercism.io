# Luhn

Welcome to Luhn on Exercism's C Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

At the Global Verification Authority, you've just been entrusted with a critical assignment.
Across the city, from online purchases to secure logins, countless operations rely on the accuracy of numerical identifiers like credit card numbers, bank account numbers, transaction codes, and tracking IDs.
The Luhn algorithm is a simple checksum formula used to help identify mistyped numbers.

A batch of identifiers has just arrived on your desk.
All of them must pass the Luhn test to ensure they're legitimate.
If any fail, they'll be flagged as invalid, preventing mistakes such as incorrect transactions or failed account verifications.

Can you ensure this is done right? The integrity of many services depends on you.

## Instructions

Determine whether a number is valid according to the [Luhn formula][luhn].

The number will be provided as a string.

## Validating a number

Strings of length 1 or less are not valid.
Spaces are allowed in the input, but they should be stripped before checking.
All other non-digit characters are disallowed.

## Examples

### Valid credit card number

The number to be checked is `4539 3195 0343 6467`.

The first step of the Luhn algorithm is to start at the end of the number and double every second digit, beginning with the second digit from the right and moving left.

```text
4539 3195 0343 6467
↑ ↑  ↑ ↑  ↑ ↑  ↑ ↑  (double these)
```

If the result of doubling a digit is greater than 9, we subtract 9 from that result.
We end up with:

```text
8569 6195 0383 3437
```

Finally, we sum all digits.
If the sum is evenly divisible by 10, the original number is valid.

```text
8 + 5 + 6 + 9 + 6 + 1 + 9 + 5 + 0 + 3 + 8 + 3 + 3 + 4 + 3 + 7 = 80
```

80 is evenly divisible by 10, so number `4539 3195 0343 6467` is valid!

### Invalid Canadian SIN

The number to be checked is `066 123 478`.

We start at the end of the number and double every second digit, beginning with the second digit from the right and moving left.

```text
066 123 478
 ↑  ↑ ↑  ↑  (double these)
```

If the result of doubling a digit is greater than 9, we subtract 9 from that result.
We end up with:

```text
036 226 458
```

We sum the digits:

```text
0 + 3 + 6 + 2 + 2 + 6 + 4 + 5 + 8 = 36
```

36 is not evenly divisible by 10, so number `066 123 478` is not valid!

[luhn]: https://en.wikipedia.org/wiki/Luhn_algorithm

## Source

### Created by

- @vlzware

### Contributed to by

- @h-3-0
- @patricksjackson
- @QLaille
- @ryanplusplus
- @wolf99

### Based on

The Luhn Algorithm on Wikipedia - https://en.wikipedia.org/wiki/Luhn_algorithm