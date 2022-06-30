# List Operations

Welcome to List Operations on Exercism's AWK Track.
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

## New AWK concepts

Refer to the [`matrix` exercise instructions][ex-matrix] to refresh on some interesting GNU awk language features.

### Dynamic function invocation

awk functions are not first-class objects: they can't get passed around like some other languages allow.
However, a variable can hold the _name_ of a function (a string), and the function is invoked using a special `@varname(args)` notation.
An example:

```awk
function greet(name) {
    print "Hello, " name
}
BEGIN {
    greet("John")       # => "Hello, John"

    f = "greet"
    f("Bill")           # => error
    @f("Bill")          # => "Hello, Bill"
}
```
This is described in [Indirect Function Calls][indirect].
This is a `gawk` extension.


[ex-matrix]: https://exercism.org/tracks/awk/exercises/matrix
[indirect]: https://www.gnu.org/software/gawk/manual/html_node/Indirect-Calls.html

## Source

### Created by

- @glennj