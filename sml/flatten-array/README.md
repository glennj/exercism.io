# Flatten Array

Welcome to Flatten Array on Exercism's Standard ML Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Take a nested list and return a single flattened list with all values except nil/null.

The challenge is to write a function that accepts an arbitrarily-deep nested list-like structure and returns a flattened structure without any nil/null values.

For example:

input: [1,[2,3,null,4],[null],5]

output: [1,2,3,4,5]

You can think of this data structure as a [`Rose Tree`](https://en.wikipedia.org/wiki/Rose_tree). You are given a data type `'a tree` that represents this data structure.

**Notes**
- For this exercise `Empty` represents `null`
- The nested list is represented by `List [...]`.
- The input example is represented by `List [Elem 1, List [Elem 2, Elem 3, Empty, Elem 4], List [Empty], Elem 5]`

## Source

### Contributed to by

- @iHiD
- @kytrinyx
- @mcmillhj-wta
- @sjwarner-bp
- @snahor
- @sshine

### Based on

Interview Question - https://reference.wolfram.com/language/ref/Flatten.html