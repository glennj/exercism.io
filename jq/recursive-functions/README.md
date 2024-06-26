# Recursive Functions

Welcome to Recursive Functions on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Recursion

**Recursive functions** are functions that call themselves.

A _recursive function_ needs to have at least one _base case_ and at least one _recursive case_.

A **base case** returns a value without calling the function again.
A **recursive case** calls the function again, modifying the input so that it will at some point match the base case.

Here is an example that counts the elements of an array.

```jq
def count:
  if length == 0 then
    0                       # base case
  else
    1 + (.[1:] | count)     # recursive case
  end;

([] | count),           # => 0
([11, 22, 33] | count)  # => 3
```

A _recursive function_ can have many _base cases_ and/or many _recursive cases_.
For example [the Fibonacci sequence][wiki-fibonacci] is a recursive sequence with two _base cases_.

```jq
def fibonacci:
  if . == 0 then
    0
  elif . == 1 then
    1
  else
    (. - 1 | fibonacci) + (. - 2 | fibonacci)
  end;

10 | fibonacci          # => 55
```

Counting the number of occurrences of some given value `x` in a list has two _recursive cases_.

```jq
def count_occurrences(x):
  if length == 0 then
    0
  elif first == x then
    1 + (.[1:] | count_occurrences(x))
  else
    (.[1:] | count_occurrences(x))
  end;

[11, 22, 33, 22, 44] | count_occurrences(22)    # => 2
```

In practice, iterating over lists and other enumerable data structures is most often done using builtin functions,
such as `map` and `reduce`, or by [using streams][map-implementation] like `[.[] | select(...)]`.
Under the hood, some builtins are [implemented using recursion][walk-implementation].

[map-implementation]: https://github.com/jqlang/jq/blob/jq-1.7/src/builtin.jq#L3
[walk-implementation]: https://github.com/jqlang/jq/blob/jq-1.7/src/builtin.jq#L248
[wiki-fibonacci]: https://en.wikipedia.org/wiki/Fibonacci_number

## Instructions

In this exercise you're going to implement some recursive functions.

## 1. Implement a function to add the numbers in an array

We'll re-implement the builtin `add` filter to practice recursion.
The _base case_ is that an empty array has a sum of **zero**.

Implement it yourself with a recursive function; don't use the builtin `add`.

```jq
[5, 4, 6, 10] | array_add     # => 25
```

## 2. Reverse an array

We'll re-implement the builtin `reverse` filter.
The _base case_ is that an empty array reversed is an empty array.

Implement it yourself with a recursive function; don't use the builtin `reverse`.

```sh
[5, 4, 6, 10] | array_reverse   # => [10, 6, 4, 5]
```

## 3. Map an array

We'll re-implement the builtin `map` filter.
The function takes a filter as a parameter, run that filter for each element of the input array, and return the outputs in a new array.
The _base case_ is that an empty array maps to an empty array.

Implement it yourself with a recursive function; don't use the builtin `map`.

```sh
[5, 4, 6, 10] | array_map(. + 10)   # => [15, 14, 16, 20]
```

## Source

### Created by

- @glennj