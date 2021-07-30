# List Operations

Welcome to List Operations on Exercism's Bash Track.
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

This is the first exercise we've seen where the solution we're writing
is not a "main" script. We're writing a library to be "source"d into
other scripts that will invoke our functions.

## Bash namerefs

This exercise requires the use of `nameref` variables. This requires a bash
version of at least 4.0. If you're using the default bash on MacOS, you'll
need to install another version: see [Installing Bash](https://exercism.io/tracks/bash/installation)

Namerefs are a way to pass a variable to a function _by reference_. That
way, the variable can be modified in the function and the updated value is
available in the calling scope. Here's an example:
```bash
prependElements() {
    local -n __array=$1
    shift
    __array=( "$@" "${__array[@]}" )
}

my_array=( a b c )
echo "before: ${my_array[*]}"    # => before: a b c

prependElements my_array d e f
echo "after: ${my_array[*]}"     # => after: d e f a b c
```

## Source

### Created by

- @glennj

### Contributed to by

- @kotp