# Flatten Array

Welcome to Flatten Array on Exercism's Tcl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Take a nested list and return a single flattened list with all values except nil/null.

The challenge is to write a function that accepts an arbitrarily-deep nested list-like structure and returns a flattened structure without any nil/null values.

For example:

input: [1,[2,3,null,4],[null],5]

output: [1,2,3,4,5]

## "Null" in Tcl

Tcl does not have a `null` or `nil` value.
In practice, the empty string is often used to indicate "no value".
But that can be tricky in situations where an empty string is actually a valid value, for example in a list representing a row of CSV data.

For this exercise, treat the empty string as a null value that is to be discarded.

## Source

### Created by

- @glennj

### Contributed to by

- @sshine

### Based on

Interview Question - https://reference.wolfram.com/language/ref/Flatten.html