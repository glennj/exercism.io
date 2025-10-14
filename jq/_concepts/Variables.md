# Introduction

Recall that a `jq` program is a _pipeline of expressions_.
**Variable assigmnent** follows this rule: a _variable assignment_ is an expression that looks like:

```jq
... | expr as $varname | ...
```

## The filter outputs the input

Like the _identity expression_ `.`, the _variable assignment_'s output is the same as its input.
Setting the variable's value is a side-effect.

Example, showing `.` after the assignment is the same as the initial input:

```sh
$ jq -c -n '42 | (. * 2) as $double | [., $double]'
[42,84]
```

## Variable scope

The scope of the variable is "lexical" -- the variable will be in scope for the rest of the pipeline.
Variables defined in functions (we'll get to functions later) are "local" to the function.

## Variable names

The variable must begin with `$`.
The rest of the variable name is an **identifier**: starts with a letter or underscore followed by letters, numbers or underscores.

## Destructuring assignment

From the manual:

> Multiple variables may be declared using a single `as` expression by providing a pattern that _matches the structure of the input_ (this is known as "destructuring").

Examples:

```jq
[1, 2, 3] as [$a, $b, $c]
| [$a, $b, $c]     # => [1, 2, 3]
```

```jq
{"foo": "bar", "len": [10, 20]} as {foo: $f, len: [$h, $w]}
| [$f, $h, $w]     # => ["bar", 10, 20]
```

Matching the variable name to the _object key_ offers a shortcut:

```jq
{"x": 4, "y": 7} as {$x, $y}
| [$x, $y]         # => [4, 7]
```

```jq
{"foo": "bar", "len": [10, 20]} as {$foo, $len}
| [$foo, $len]     # => ["bar", [10, 20]]
```

<!-- prettier-ignore -->
~~~~exercism/note
The same shortcut works in the reverse sense for _object construction_

```jq
4 as $x | 7 as $y | {$x, $y}    # => {"x":4, "y": 7}
```
~~~~

<!-- prettier-ignore-end -->

## Constants

As shown in the above example, variables are handy for storing constants.
It's better to give constants a name rather than have "magic numbers" show up in the code.

## Often variables are not needed

It is often more readable to omit variables.

Consider calculating the average of a list of numbers

```jq
[2, 4, 8, 7]
| add as $sum
| length as $len
| $sum / $len
```

compared to

```jq
[2, 4, 8, 7] | add / length
```

This highlights one of the most powerful aspects of `jq`: the input to a filter is provided to _every_ sub-expression:

- the input array `[2,4,8,7]` is passed to `add` (which sums the numbers by applying the `+` operator to all the elements)
- _in parallel_, `[2,4,8,7]` is also passed to `length`
- when these sub-expressions complete, their results are then divided, returning the average value of the numbers in the input array.
