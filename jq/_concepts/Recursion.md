# About

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

## Infinite recursion

Recursive functions, if implemented incorrectly, might never return their result.
This can be problematic because each time a function is called, a reference is stored in memory where the VM should return the result (on the [call stack][wiki-call-stack]).
If a recursive function calls itself infinitely, it is possible to run out of memory, and causing the VM to crash with a [stack overflow error][wiki-stack-overflow].

This problem of infinite recursion can be caused by:

- Forgetting to implement a base case.
- Not defining the base case as the first clause.
- Not modifying the argument properly when doing the recursive call, and thus never reaching the base case.

## Tail call optimization

As [described in Wikipedia][wiki-tail-call]:

> A tail call is a subroutine call performed as the final action of a procedure.
> If the target of a tail is the same subroutine, the subroutine is said to be tail recursive.

Consider the implementation of `fibonacci` implemented above.
The recursive case calls itself twice.
The last action that the function needs to perform is addition.
Since the last action is not a call to itself, that function is recursive, but not tail recursive.

Some languages implement _tail call optimization_, such that the tail call can reuse the current stack frame instead of having to add a new frame to the [call stack][wiki-call-stack].
This can have performance benefits above guarding against the dreaded [stack overflow][wiki-stack-overflow].

`jq` does implement tail call optimization, _but only for 0-arity functions_.
The way to take advantage of this is to define an "inner" 0-arity function that takes some kind of state as input.
The inner function can modify the state as required, and pass the new state to the tail recursive call.
The outer function creates the initial state and passes it to the inner function to start the recursion.

A tail-recursive `fibonacci` function could be implemented by keeping the series calculated so far as part of the state.

```jq
def fibonacci:
  def _fib:
    # input: {limit: number, series: array}
    if (.series | length - 1) >= .limit then
      .series[.limit]                                   # base case
    else
      .series += [.series[-1] + .series[-2]] | _fib     # recursive case
    end;
  {limit: ., series: [0, 1]} | _fib;

10 | fibonacci          # => 55
```

[map-implementation]: https://github.com/jqlang/jq/blob/jq-1.7/src/builtin.jq#L3
[walk-implementation]: https://github.com/jqlang/jq/blob/jq-1.7/src/builtin.jq#L248
[wiki-fibonacci]: https://en.wikipedia.org/wiki/Fibonacci_number
[wiki-tail-call]: https://en.wikipedia.org/wiki/Tail_call
[wiki-call-stack]: https://en.wikipedia.org/wiki/Call_stack
[wiki-stack-overflow]: https://en.wikipedia.org/wiki/Stack_overflow
