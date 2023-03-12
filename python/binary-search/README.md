# Binary Search

Welcome to Binary Search on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

You have stumbled upon a group of mathematicians who are also singer-songwriters.
They have written a song for each of their favorite numbers, and, as you can imagine, they have a lot of favorite numbers.

You are curious to hear the song for your favorite number, but with so many songs to wade through, finding the right song could take a while.
Fortunately, they have organized their songs in a playlist sorted by the title — which is simply the number that the song is about.

You realize that you can use a binary search algorithm to quickly find a song given the title.

## Instructions

Your task is to implement a binary search algorithm.

A binary search algorithm finds an item in a list by repeatedly splitting it in half, only keeping the half which contains the item we're looking for.
It allows us to quickly narrow down the possible locations of our item until we find it, or until we've eliminated all possible locations.

```exercism/caution
Binary search only works when a list has been sorted.
```

The algorithm looks like this:

- Divide the sorted list in half and compare the middle element with the item we're looking for.
- If the middle element is our item, then we're done.
- If the middle element is greater than our item, we can eliminate that number and all the numbers **after** it.
- If the middle element is less than our item, we can eliminate that number and all the numbers **before** it.
- Repeat the process on the part of the list that we kept.

Here's an example:

Let's say we're looking for the number 23 in the following sorted list: `[4, 8, 12, 16, 23, 28, 32]`.

- We start by comparing 23 with the middle element, 16.
- Since 23 is greater than 16, we can eliminate the left half of the list, leaving us with `[23, 28, 32]`.
- We then compare 23 with the new middle element, 28.
- Since 23 is less than 28, we can eliminate the right half of the list: `[23]`.
- We've found our item.

## Exception messages

Sometimes it is necessary to [raise an exception](https://docs.python.org/3/tutorial/errors.html#raising-exceptions). When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging. For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types](https://docs.python.org/3/library/exceptions.html#base-classes), but should still include a meaningful message.

This particular exercise requires that you use the [raise statement](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) to "throw" a `ValueError` when the given value is not found within the array. The tests will only pass if you both `raise` the `exception` and include a message with it.

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:

```python
# example when value is not found in the array.
raise ValueError("value not in array")
```

## Source

### Created by

- @michaelkunc

### Contributed to by

- @AnAccountForReportingBugs
- @behrtam
- @BethanyG
- @cmccandless
- @Dog
- @fortrieb
- @ikhadykin
- @kytrinyx
- @mpatibandla
- @N-Parsons
- @patricksjackson
- @pheanex
- @subkrish
- @tqa236

### Based on

Wikipedia - https://en.wikipedia.org/wiki/Binary_search_algorithm