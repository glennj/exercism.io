# About

JSON defines an **array** as:

> An array is an ordered collection of values.
> An array begins with `[` left bracket and ends with `]` right bracket.
> Values are separated by `,` comma.

Arrays can contain zero or more of _any kind_ of JSON values.
Array elements do not need to be all the same type.

## Creating arrays

Use brackets to collect elements into an array.

```jq
[1, 2, 3]
```

The elements of a _stream_ can be captured into an array by enclosing it in brackets.

```jq
range(5)     # => a stream of 5 numbers
[range(5)]   # => the array [0, 1, 2, 3, 4]
```

## Size

The `length` function returns the number of elements in an array.

## Indexing and slicing

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

## Convert to a stream of elements

There are circumstances where we want to stream the elements of an array individually.
For this we use `.[]`.

Suppose we want a stream of the lengths of the strings in an array.

```jq
["a", "be", "cat", "door"] | length        # => 4 -- the number of elements in the array
["a", "be", "cat", "door"] | .[] | length  # => 1, 2, 3, 4 -- the lengths of each element
```

The empty brackets can be placed beside the previous expression, omitting the pipe.

```jq
["a", "be", "cat", "door"][] | length      # => 1, 2, 3, 4 -- the lengths of each element
# ........................^^
```

## Concatenating and Subtracting

Use `+` to concatenate arrays.

```jq
[1, 2, 3] + [4, 5]          # => [1, 2, 3, 4, 5]
```

The `-` operator removes elements in the right-hand array from the left-hand array.

```jq
[range(10)] - [2, 4, 6]     # => [0, 1, 3, 5, 7, 8, 9]
```

## Iterating

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

## Membership

We can test that an array contains an element with `index`.
This function returns the index of the given element if it's in the array;
otherwise the funtion returns `null`.

```jq
["carrots", "beef", "potatoes"] | index("beef")    # => 1
["carrots", "beef", "potatoes"] | index("bees")    # => null
```

There is an inverse function called `IN` (yes, in capital letters).
The wrinkle with `IN` is that its argument is not an array, but a _stream_.

```jq
"beef" | IN(["carrots", "beef", "potatoes"][])    # => true
"bees" | IN(["carrots", "beef", "potatoes"][])    # => false
```

## Other functions (refer to the manual for details)

- Ruby has a handy `each_with_index` method that pairs the element value with its index.
  Python has `enumerate`.
  `jq` has something similar, named `to_entries`.
  This transforms the array to an array of `{key: index, value: value}` objects.

  ```jq
  [11, 22, 33, 44] | to_entries
  # => [{key: 0, value: 11}, {key: 1, value: 22}, {key: 2, value: 33}, {key: 3, value: 44}]
  ```

- `sort` and `sort_by`
- `group_by`
- `min`, `max`, `min_by`, `max_by`
- `unique`, `unique_by`
- `flatten`
