# Shopping List

Welcome to Shopping List on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Basics

We will assume you are familiar with JSON syntax and data types.

jq works by passing the incoming JSON data through a _single expression_ (written as a _pipeline of filters_) to achieve the desired transformed data.

<!-- prettier-ignore -->
~~~~exercism/note
The `jq` language is implemented by the `jq` _program_.
This program provides several handy command-line options to control how the input is consumed and how the output is presented.

In the examples below you'll encounter:

- `-n` or `--null-input`

    Normally the `jq` program is given a file to read, or you send data to its input.
    The `--null-input` option allows you to generate JSON data without any inputs.

- `-c` or `--compact-output`

    `jq` pretty-prints its output by default.
    It it extremely useful for humans to view the data when it's nicely formatted.
    However that's not necessary for machines: the `--compact-output` option removes the formatting whitespace to minimize the size of the resulting JSON.

- `-f filename` or `--from-file filename`

    Read the `jq` program from `filename` instead of providing it on the command line.
    `sed` and `awk` both use the `-f` option for the same purpose.
    You will see this used in the test scripts for the practice exercises.

The rest of this lesson will focus on the `jq` _language_.
~~~~

<!-- prettier-ignore-end -->

### Filters and Pipes

**Filters** are also known as **Expressions**.

A _filter_ takes an input and produces an output.
Like the way you work in a unix shell, you can join _filters_ with a pipe `|` to connect the output of one _filter_ to the input of another.

### The Identity Filter: `.`

This is the simplest _filter_.
It simply passes its input to its output.

```sh
$ echo '[1, 2, 3]' | jq '.'
[
  1,
  2,
  3
]
```

### Arrays

This will be quick introduction to working with **arrays**.
We will cover this topic in greater detail later.

_Array_ elements are accessed with brackets, and are zero-indexed.

```sh
$ echo '[10, 20, 30]' | jq '.[1]'
20
```

A _filter_ can build an _array_ by wrapping an expression in `[` and `]`

- with a known list of elements:

  ```sh
  jq -n '[1, 2, 3]'
  ```

- to collect a stream of elements: for example,
  `range` is a function that outputs a stream of numbers

  ```sh
  $ jq -n 'range(10; 70; 15)'
  10
  25
  40
  55
  ```

  Using `[]` collects the results of the expression into an array

  ```sh
  $ jq -c -n '[range(10; 70; 15)]'
  [10,25,40,55]
  ```

### Comma is an operator

The _comma_ is not just syntax that separates _array_ elements.
**Comma** is an **operator** that joins streams.

For example `[1, 2, 3]` is a _filter_ that uses the _array_ constructor `[]` to collect the result of joining the three expressions `1`, `2` and `3`.

Did you notice the semi-colons in `range(10; 70; 15)` above?
Because _commas_ have a specific purpose in the `jq` language, functions that take multiple arguments use semi-colons to separate the arguments.

### Objects

A quick introduction to **objects**.

Similar to many programming languages, use dots to access _object_ properties

```sh
$ echo '{"foo": {"bar": "qux"}}' | jq '.foo.bar'
"qux"
```

<!-- prettier-ignore -->
~~~~exercism/note
Brackets can be used for _objects_ too, but then quotes are needed for string literals.
This is one method to work with keys containing spaces.

```sh
$ echo '{"foo bar": "qux"}' | jq '.["foo bar"]'
"qux"
```
~~~~

<!-- prettier-ignore-end -->

You can construct an _object_ with `{}` and `key: value` pairs.
Quotes are not required around _keys_ that are "simple" strings.

```sh
jq -n '{question: (6 * 9), answer: 42}'
```

outputs

```none
{
  "question": 54,
  "answer": 42
}
```

To treat the _key_ as an _expression_, you must wrap it in parentheses (the following also outputs the same as above).

```sh
echo '[{"key":"question", "value":54}, {"key":"answer", "value":42}]' \
| jq '{(.[0].key): .[0].value, (.[1].key): .[1].value}'
```

### Pipelines

For example, given `file.json` containing

```json
{
  "key1": "value1",
  "key2": [5, 15, 25]
}
```

Let's calculate the length of the key2 _array_:

```sh
$ jq '.key2 | length' file.json
3
```

We're _piping_ the output of the `.key2` expression as the input to `length` which unsurprisingly outputs the number of elements in the _array_.

### Filters can ignore their input

In this example, the input JSON data is ignored and has no impact on the output:

```sh
$ echo '{"answer": 42}' | jq '6 * 9'
54
```

### Filters can output streams of data

A _filter_ can output more than one value.
For example, the `.[]` _filter_ outputs each element of an _array_ as a separate value:

```sh
$ jq -n -c '[1, 2, 3]'
[1,2,3]

$ jq -n -c '[1, 2, 3] | .[]'
1
2
3
```

Piping such a _filter_ into another will execute the 2nd _filter_ **_for each value_**:

```sh
$ jq -n -c '[1, 2, 3] | .[] | . * 2'
2
4
6
```

This is like implicit iteration.
Once you understand this technique, you'll realize very powerful `jq` _filters_ can be very concise.

### Parentheses

**Parentheses** are used to group sub-expressions together to enforce the order of operations, just like in other languages.
In `jq`, the need for them can appear to be somewhat surprising.

For example, let's say we want to construct an _array_ with 2 elements: the square root of 9; and _e_ raised to the power 1.
The two individual expressions are `9 | sqrt` and `1 | exp`.
We expect the output to be the _array_ `[3, 2.7...]`

```jq
$ jq -n '[ 9|sqrt, 1|exp ]'
[
  20.085536923187668,
  2.718281828459045
]
```

Why didn't we get what we expected? `jq` interprets that like this:

```jq
[ ((9|sqrt), 1) | exp ]
```

`jq` builds a stream of two elements (`3` and `1`) which are each given to `exp`.

We need to ensure that `exp` only takes one number as input.
In other words, we need to enforce that the pipe is evaluated before the comma.

```jq
$ jq -n '[ 9|sqrt, (1|exp) ]'
[
  3,
  2.718281828459045
]
```

### Functions and Operators

Without going into great depth (functions will be a topic for another exercise), here are some useful builtin functions:

- `length`

  Given an array as input, output the number of elements in the array.

  ```sh
  $ jq -n '[10, 20, 30, 40] | length'
  4
  ```

- `+`

  This operator does different things depending on the type of its operands:
  it adds numbers,
  it concatenates strings,
  it appends arrays,
  it merges objects.

  ```sh
  $ jq -c -n '
      3 + 4,
      "foo" + "bar",
      ["a", "b"] + ["c"],
      {"m": 10} + {"n": 20}
  '
  7
  "foobar"
  ["a","b","c"]
  {"m":10,"n":20}
  ```

  `add` is a function that takes an array and returns an item with all the elements added together using the rules of `+`.
  `[1, 2, 3] | add` outputs `6`.

- `map`

  Given an array as input and a filter as an argument, output an array where the filter is applied to each element

  ```sh
  $ jq -c -n '[10, 20, 30, 40] | map(. / 5)'
  [2,4,6,8]
  ```

- `select`

  Given _some_ input and a filter as an argument:

  - if the filter applied to the argument results in a _true_ value, output the input unchanged
  - otherwise, output _nothing_ (not the `null` value, truly no output)

  For example, given some numbers, select the ones divisible by 3

  ```sh
  $ jq -n 'range(10) | select(. % 3 == 0)'
  0
  3
  6
  9
  ```

  Recall that `range` outputs a _stream_ of numbers.
  `select` will be invoked once per each number.
  Only the numbers "passing" the expression are output.

  You often need to select elements of an array.
  There are a couple of ways to do this.

  With the input `["Anne", "Bob", "Cathy", "Dave"]`, select the names having length 4.

  - use `map` and `select` together

    ```jq
    map(select(length == 4))
    ```

  - explode the array into elements, `select` on that stream, and collect the results

    ```jq
    [ .[] | select(length == 4) ]
    ```

### Comments

Comments start with a `#` character and continue to the end of the line.

## Instructions

You have a sudden craving for pancakes.
As a modern technically-inclined person, you write a shopping list for pancake ingredients in JSON format.
The structure of the list is:

```json
{
  "name": "name of shopping list",
  "ingredients": [ ...list of ingredients... ],
  "optional ingredients": [ ...list of ingredients... ]
}
```

An ingredient is represented as a JSON object, like:

```json
{
  "item": "flour",
  "amount": {
    "quantity": 2,
    "unit": "cup"
  }
}
```

An ingredient has an optional property named `"substitute"` that holds a
string of another item that can be used instead.

Let's see how we can examine the shopping list with `jq`

## 1. Extract the shopping list name

Write an expression that outputs the "name" element of the shopping list.

## 2. Count how many ingredients are needed

Only count the "required" ingredients. Don't include the optional ingredients.

## 3. How much sugar?

Write an expression that outputs the amount of sugar.
Just the numeric part is wanted.

## 4. Map the ingredients which can be substituted

Some of the ingredients can be substituted (if you don't have ingredient X you can use Y).
Output a JSON object mapping the recommended ingredient to its substitution.
Include the optional ingredients in the mapping.

## Source

### Created by

- @glennj