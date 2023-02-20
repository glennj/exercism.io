# Largest Series Product

Welcome to Largest Series Product on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a string of digits, calculate the largest product for a contiguous substring of digits of length n.

For example, for the input `'1027839564'`, the largest product for a series of 3 digits is 270 `(9 * 5 * 6)`, and the largest product for a series of 5 digits is 7560 `(7 * 8 * 3 * 9 * 5)`.

Note that these series are only required to occupy *adjacent positions* in the input; the digits need not be *numerically consecutive*.

For the input `'73167176531330624919225119674426574742355349194934'`,
the largest product for a series of 6 digits is 23520.

For a series of zero digits, the largest product is 1 because 1 is the multiplicative identity.
(You don't need to know what a multiplicative identity is to solve this problem;
it just means that multiplying a number by 1 gives you the same number.)

## Exception messages

Sometimes it is necessary to [raise an exception](https://docs.python.org/3/tutorial/errors.html#raising-exceptions). When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging. For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types](https://docs.python.org/3/library/exceptions.html#base-classes), but should still include a meaningful message.

This particular exercise requires that you use the [raise statement](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) to "throw" a `ValueError` when your `largest_product()` function receives invalid input. The tests will only pass if you both `raise` the `exception` and include a message with it.  Feel free to reuse your code from the `series` exercise!

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:

```python
# span of numbers is longer than number series
raise ValueError("span must be smaller than string length")

# span of number is zero or negative
raise ValueError("span must be greater than zero")

# series includes non-number input
raise ValueError("digits input must only contain digits")
```

## Source

### Created by

- @sjakobi

### Contributed to by

- @alexpjohnson
- @behrtam
- @cmccandless
- @Dog
- @fortrieb
- @iandexter
- @ikhadykin
- @kytrinyx
- @N-Parsons
- @petertseng
- @pheanex
- @tqa236

### Based on

A variation on Problem 8 at Project Euler - https://projecteuler.net/problem=8