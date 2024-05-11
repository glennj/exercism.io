# Largest Series Product

Welcome to Largest Series Product on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

You work for a government agency that has intercepted a series of encrypted communication signals from a group of bank robbers.
The signals contain a long sequence of digits.
Your team needs to use various digital signal processing techniques to analyze the signals and identify any patterns that may indicate the planning of a heist.

## Instructions

Your task is to look for patterns in the long sequence of digits in the encrypted signal.

The technique you're going to use here is called the largest series product.

Let's define a few terms, first.

- **input**: the sequence of digits that you need to analyze
- **series**: a sequence of adjacent digits (those that are next to each other) that is contained within the input
- **span**: how many digits long each series is
- **product**: what you get when you multiply numbers together

Let's work through an example, with the input `"63915"`.

- To form a series, take adjacent digits in the original input.
- If you are working with a span of `3`, there will be three possible series:
  - `"639"`
  - `"391"`
  - `"915"`
- Then we need to calculate the product of each series:
  - The product of the series `"639"` is 162 (`6 × 3 × 9 = 162`)
  - The product of the series `"391"` is 27 (`3 × 9 × 1 = 27`)
  - The product of the series `"915"` is 45 (`9 × 1 × 5 = 45`)
- 162 is bigger than both 27 and 45, so the largest series product of `"63915"` is from the series `"639"`.
  So the answer is **162**.

## Exception messages

Sometimes it is necessary to [raise an exception](https://docs.python.org/3/tutorial/errors.html#raising-exceptions). When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging. For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types](https://docs.python.org/3/library/exceptions.html#base-classes), but should still include a meaningful message.

This particular exercise requires that you use the [raise statement](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) to "throw" a `ValueError` when your `largest_product()` function receives invalid input. The tests will only pass if you both `raise` the `exception` and include a message with it.  Feel free to reuse your code from the `series` exercise!

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:

```python
# span of numbers is longer than number series
raise ValueError("span must be smaller than string length")

# span of number is negative
raise ValueError("span must not be negative")

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