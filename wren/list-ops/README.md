# List Ops

Welcome to List Ops on Exercism's Wren Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Implement basic list operations.

In functional languages list operations like `length`, `map`, and
`reduce` are very common. Implement a series of basic list operations,
without using existing functions.

The precise number and names of the operations to be implemented will be
track dependent to avoid conflicts with existing names, but the general
operations you will implement include:

* `append` (*given two lists, add all items in the second list to the end of the first list*);
* `concatenate` (*given a series of lists, combine all items in all lists into one flattened list*);
* `filter` (*given a predicate and a list, return the list of all items for which `predicate(item)` is True*);
* `length` (*given a list, return the total number of items within it*);
* `map` (*given a function and a list, return the list of the results of applying `function(item)` on all items*);
* `foldl` (*given a function, a list, and initial accumulator, fold (reduce) each item into the accumulator from the left using `function(accumulator, item)`*);
* `foldr` (*given a function, a list, and an initial accumulator, fold (reduce) each item into the accumulator from the right using `function(item, accumulator)`*);
* `reverse` (*given a list, return a list with all the original items, but in reversed order*);

## Implementing List operations

The goal of this exercise is to implement some of List methods yourself.
It's natural to use a List object in the backend to hold the data. 
Keep your hands off the List methods (like `where`, `map`, etc).

There are a some "primitive" List operations that are unavoidable, such as:

* slicing: `list[from..to]`
* adding: `list.add(item)`

## Iterator protocol

The list operations to implement are all about iterating in various ways.
Read about [the iterator protocol][iter].
Then you'll be able to do:

```wren
var list = ListOps.new([1, 2, 3, 4])
for (item in list) {
    do_something_with(item)
}
```

[iter]: https://wren.io/control-flow.html#the-iterator-protocol

## Source

### Created by

- @glennj