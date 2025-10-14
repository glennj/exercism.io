# About

Suppose we want to sum an array of numbers.
There are many ways to accomplish this goal.

- In many languages, this problem is expressed as a loop with an accumulator variable.
- This problem can be written as a recursive function.
  In pseudo-code, we might have this.

  ```none
  function Add(X, Sum=0):
    if X is empty then
      return Sum
    else
      return Add(rest(X), Sum + first(X))
    end
  end
  ```

  This method of dividing the problem into smaller pieces can also be described as "reducing towards the base case."

**Reduce** is a way to combine all the elements of a data structure into a single value.
The process iterates over the data structure, applying a function to each element to update the accumulated result.

In `jq`, this process is implemented in the [`reduce` filter][jq-man-reduce].
In other languages, it might be called "fold", "fold-left", "inject", or "aggregate".

The `jq` `reduce` expression looks like this.

```jq
reduce STREAM_EXPRESSION as $var (INITIAL_VALUE; UPDATE_EXPRESSION)
```

- `STREAM_EXPRESSION` is a _stream_ of items, each stored in the `$var` variable in turn.
  - Recall, to stream an array, use the [iterator filter `.[]`][jq-man-iterator]: `$myArray | .[]`.
- `INITIAL_VALUE` is the starting value of the accumulated result (known as the "accumulator").
- The `UPDATE_EXPRESSION` combines ("folds") the current value (`$var`) into the accumulator.
  - In the context of this expression, `.` is the value of the accumulator.
  - The output of the expression is stored into the accumulator for use in the next iteration.
  - After the last iteration, the accumulated result is the output of `reduce`.

Let's look at an example: adding up the numbers in an array.
The `add` filter does just this, but we'll see how to implement it.

If we use `[10, 20, 30, 40]` as the input, and taking zero as the initial state, this is what each step looks like.

| \#  | state | element | reducer | result |
| :-: | :---: | :-----: | :-----: | :----: |
|  1  |   0   |   10    | 0 + 10  |   10   |
|  2  |  10   |   20    | 10 + 20 |   30   |
|  3  |  30   |   30    | 30 + 30 |   60   |
|  4  |  60   |   40    | 60 + 40 |  100   |

In `jq` syntax, that looks like this code.

```jq
0 + 10 | . + 20 | . + 30 | . + 40
```

We can express that with the `reduce` filter.

```jq
[10, 20, 30, 40] | reduce .[] as $n (0; . + $n)     # => 100
```

<!-- prettier-ignore -->
~~~~exercism/note
The `add` builtin is actually [implemented with `reduce`][jq-code-add], but uses "null" as the initial state (any data type can be added to null).

```jq
def add: reduce .[] as $x (null; . + $x);
```

[jq-code-add]: https://github.com/jqlang/jq/blob/jq-1.7/src/builtin.jq#L11
~~~~

<!-- prettier-ignore-end -->

## Some things to keep in mind

- In the reducing expression, `.` is the accumulator.
  If the input is some object that you need to reference inside the reducing function, you need to store it in a variable.

  ```jq
  {"apple": 10, "banana": 16, "carrot": 4}
  | . as $obj
  | reduce (keys | .[]) as $key (0; . + $obj[$key])     # => 30
  ```

- The accumulator can be of any type of data.
  For example you may want to reverse an array.

  ```jq
  ["A", "B", "C", "D"]
  | reduce .[] as $elem ([]; [$elem] + .)       # => ["D", "C", "B", "A"]
  ```

[jq-man-reduce]: https://jqlang.github.io/jq/manual/v1.7/#reduce
[jq-man-iterator]: https://jqlang.github.io/jq/manual/v1.7/#array-object-value-iterator
