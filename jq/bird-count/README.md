# Bird Count

Welcome to Bird Count on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Arrays

JSON defines an **array** as:

> An array is an ordered collection of values.
> An array begins with `[` left bracket and ends with `]` right bracket.
> Values are separated by `,` comma.

Arrays can contain zero or more of _any kind_ of JSON values.
Array elements do not need to be all the same type.

### Creating arrays

Use brackets to collect elements into an array.

```jq
[1, 2, 3]
```

The elements of a _stream_ can be captured into an array by enclosing it in brackets.

```jq
range(5)     # => a stream of 5 numbers
[range(5)]   # => the array [0, 1, 2, 3, 4]
```

### Size

The `length` function returns the number of elements in an array.

### Indexing and slicing

Array indexing is zero-based.

Retrieve an element from an array with a bracket expression:
`.[2]` gets the third element.

Negative indexes count backwards from the end of the array:
`.[-1]` gets the last element; `.[-2]` is the second last.

A **slice** is a sub-sequence of the array.
`.[10:15]` returns 5 elements starting from index 10; the end index is _not included_.

There are some convenience functions:

- `first` gets the first element.
- `last` gets the last element.
- `nth(n)` gets the element at index `n`.

### Iterating

`jq` provides many functions to cover common iteration functionality:

- `map(expr)` returns a new array where the `expr` is applied to each element in turn.

  ```jq
  [1, 2, 3] | map(. * 10)    # => [10, 20, 30]

  ```

- `select(expr)` is a function that applies the `expr` to a _single value_;
  if the result of the expression is true, then the value is returned;
  if the result is false, _nothing_ is returned -- not `null`, actually nothing.

  To apply that to an array, combine it with `map`.

  ```jq
  [range(10)] | map(select(. % 2 == 0))    # => [0, 2, 4, 6, 8]
  ```

  Alternately, apply `select` to a _stream_ and collect the results.

  ```jq
  [range(10) | select(. % 2 == 0)]    # => [0, 2, 4, 6, 8]
  ```

- `any(condition)` and `all(condition)` return a boolean value whether any/all of the elements in the array pass the condition.

  ```jq
  [1, 2, 3, 4] | any(. > 4)    # false
  [1, 2, 3, 4] | any(. >= 4)   # true

  [1, 2, 3, 4] | all(. >= 4)   # false
  [1, 2, 3, 4] | all(. <= 4)   # true
  ```

## Instructions

You are an avid bird watcher that keeps track of how many birds have visited your garden in the last seven days.

You have five tasks, all dealing with the numbers of birds that visited your garden.

## 1. Check what the counts were last week

You store your bird counts in JSON format.
A "main" array stores arrays of weekly counts.
Each weekly count is an array of seven integers, one number for each day of the week.
You always append new weeks at the end of the main array.
Implement the filter that returns last week's counts.

```sh
$ cat bird-counting-data.json
[
  ...,
  [0, 2, 5, 3, 7, 8, 4],
  [4, 5, 9, 10, 9, 4, 3]
]

$ jq -f bird-count.jq < bird-counting-data.json
{
  "last_week": [0, 2, 5, 3, 7, 8, 4],
  ...
```

## 2. Check how many birds visited yesterday

Implement the filter to return how many birds visited your garden yesterday.
The bird counts are ordered by day, with the first element being the count of the oldest day, and the last element being today's count.

```jq
$ jq -f bird-count.jq < bird-counting-data.json
{
  ...
  "yesterday": 4,
  ...
```

## 3. Calculate the total number of visiting birds

Implement the filter to return the total number of birds that have visited your garden this week.

```jq
$ jq -f bird-count.jq < bird-counting-data.json
{
  ...
  "total": 44,
  ...
```

## 4. Calculate the number of busy days

Some days are busier than others.
A busy day is one where five or more birds have visited your garden.
Implement the filter to return the number of busy days this week.

```jq
$ jq -f bird-count.jq < bird-counting-data.json
{
  ...
  "busy_days": 4,
  ...
```

## 5. Check if there was a day with no visiting birds

Implement the filter that returns `true` if there was any day this week when zero birds visited the garden; otherwise, return `false`.

```jq
$ jq -f bird-count.jq < bird-counting-data.json
{
  ...
  "has_day_without_birds": false
}
```

## Source

### Created by

- @glennj