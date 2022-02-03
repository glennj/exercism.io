# List Ops

Welcome to List Ops on Exercism's Elixir Track.
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

## Elixir-specific changes

The above description of this exercise is shared between all Exercism tracks.

To be consistent with Elixir's standard library, the functions used by `foldl` and `foldr` should take the item as the first argument, and the accumulator as the second.

Additionally, in order not to conflict with the automatically imported function `Kernel.length/1`, the function counting the length will be named `count` instead.

## Slow tests

One or several of the tests of this exercise have been tagged as `:slow`, because they might take a long time to finish. For this reason, they will not be run on the platform by the automated test runner. If you are solving this exercise directly on the platform in the web editor, you might want to consider downloading this exercise to your machine instead. This will allow you to run all the tests and check the efficiency of your solution.

## Source

### Contributed to by

- @andrewsardone
- @angelikatyborska
- @Cohen-Carlisle
- @dalexj
- @devonestes
- @henrik
- @jinyeow
- @kytrinyx
- @lpil
- @neenjaw
- @parkerl
- @rubysolo
- @sotojuan
- @Teapane
- @waiting-for-dev