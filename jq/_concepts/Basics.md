# About

jq works by passing the incoming JSON data through a _single expression_ (written as a _pipeline of filters_) to achieve the desired transformed data.

## A quick introduction to `jq` command-line options

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

See [the manual][man-cli] for details about all the options.

## Filters and Pipes

**Filters** are also known as **Expressions**.

A _filter_ takes an input and produces an output.
Like the way you work in a unix shell, you can join _filters_ with a pipe `|` to connect the output of one _filter_ to the input of another.

## The Identity Filter: `.`

This is the simplest _filter_.
It simply passes its input to its output.
For example, `jq` pretty-prints by default, so passing JSON the `.` filter gives nicely formatted output for free!

```sh
$ echo '[1, 2, 3]' | jq '.'
[
  1,
  2,
  3
]
```

## Arrays

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
  [`range`][man-range] is a function that outputs a stream of numbers

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

## Comma is an operator

The _comma_ is not just syntax that separates _array_ elements.
**Comma** is an **operator** that joins streams.

For example `[1, 2, 3]` is a _filter_ that uses the _array_ constructor `[]` to collect the result of joining the three expressions `1`, `2` and `3`.

Did you notice the semi-colons in `range(10; 70; 15)` above?
Because _commas_ have a specific purpose in the `jq` language, functions that take multiple arguments use semi-colons to separate the arguments.

## Objects

A quick introduction to **objects**.

Similar to many programming languages, use dots to access _object_ properties

```sh
$ echo '{"foo": {"bar": "qux"}}' | jq '.foo.bar'
"qux"
```

Brackets can be used for _objects_ too, but then quotes are needed for string literals.
This is one method to work with keys containing spaces.

```sh
$ echo '{"foo bar": "qux"}' | jq '.["foo bar"]'
"qux"
```

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
echo '[["question", "answer"], [54, 42]]' \
| jq '{(.[0][0]): .[1][0], (.[0][1]): .[1][1]}'
```

<!-- prettier-ignore -->
~~~~exercism/note
It is quite common to want to extract a subset of keys from a large object.
For example, to extract `id` and `name` from

```json
{
    "id": 101,
    "name": "alpha widget",
    "specifications": {...}
}
```

We could write
```jq
{id: .id, name: .name}
```
But this is so common, there is shorthand syntax for it:
```jq
{id, name}
```
~~~~

<!-- prettier-ignore-end -->

## Pipelines

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

We're _piping_ the output of the `.key2` expression as the input to [`length`][man-length], which unsurprisingly outputs the number of elements in the array.

<!-- prettier-ignore -->
~~~~exercism/caution
This is an aspect of `jq` that takes some getting used to --
most (but not all) functions act like _filters_ where you pass data to the filter's input, not as an argument.
~~~~

<!-- prettier-ignore-end -->

## Filters can ignore their input

In this example, the input JSON data is ignored and has no impact on the output:

```sh
$ echo '{"answer": 42}' | jq '6 * 9'
54
```

## Filters can output streams of data

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

## Parentheses

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

## Types

From [the manual][man-types]

> jq supports the same set of datatypes as JSON - numbers, strings, booleans, arrays, objects (which in JSON-speak are hashes with only string keys), and "null".

You'll learn more about these in subsequent exercises.

## Whitespace

Whitespace is not significant in `jq`.
Use spaces/tabs/newlines as you see fit to format your code.
We are unaware of any existing `jq` style guides.

## Immutable Values

Values in `jq` are _immutable_.
Filters that modify a value will output a new value.
This implies that `jq` does not have global variables --
you'll need to get used to passing state from one filter to another.

## "Truthiness"

The values `false` and `null` are considered false. Any other value
(including the number zero and the empty string/array/object) is true.

## Functions and Operators

Without going into great depth (functions will be a topic for another exercise), here are some useful builtin functions:

- [`length`][man-length]

  Given an array as input, output the number of elements in the array.

  ```sh
  $ jq -n '[10, 20, 30, 40] | length'
  4
  ```

- [`+`][man-plus]

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

  [`add`][man-add] is a function that takes an array and returns an item with all the elements added together using the rules of `+`.
  `[1, 2, 3] | add` outputs `6`.

- [`map`][man-map]

  Given an array as input and a filter as an argument, output an array where the filter is applied to each element

  ```sh
  $ jq -c -n '[10, 20, 30, 40] | map(. / 5)'
  [2,4,6,8]
  ```

- [`select`][man-select]

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

## Comments

Comments start with a `#` character and continue to the end of the line.

[man-cli]: https://jqlang.github.io/jq/manual/v1.7/#invoking-jq
[man-types]: https://jqlang.github.io/jq/manual/v1.7/#types-and-values
[man-length]: https://jqlang.github.io/jq/manual/v1.7/#length
[man-range]: https://jqlang.github.io/jq/manual/v1.7/#range
[man-map]: https://jqlang.github.io/jq/manual/v1.7/#map-map_values
[man-plus]: https://jqlang.github.io/jq/manual/v1.7/#addition
[man-add]: https://jqlang.github.io/jq/manual/v1.7/#add
[man-select]: https://jqlang.github.io/jq/manual/v1.7/#select
