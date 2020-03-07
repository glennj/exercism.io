# List Ops

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

## The Tcl `apply` command

The test cases may look confusing. You are expected to implement this: 
```tcl
set myList {alpha beta gamma delta}
listOps::filter $myList {{word} {expr {[string length $word] == 4}}
```
Why does that last argument have so many braces?

Recall that the `proc` command is defined as:
```tcl
proc procName argList body
```

Tcl has an `apply` command:
```tcl
apply func ?arg1 arg2 ...?
```
this "func" is a two element list `{argList body}` that is essentially an
anonymous proc (or "lambda"). The `apply` command invokes that anonymous
proc, passing it the arguments it needs.

As an example, these are equivalent:
```tcl
# using proc
proc myReverse {str} {return [string reverse $str]}
puts [myReverse "Hello, World!"]

# using apply
puts [apply {{str} {string reverse $str}} "Hello, World!"]

# or, store the func in a variable
set func {{str} {string reverse $str}}
puts [apply $func "Hello, World!"]
```
Using `apply` makes it simpler to pass around blocks of code.

Ref: [`apply`](https://tcl.tk/man/tcl8.6/TclCmd/apply.htm),
[`proc`](https://tcl.tk/man/tcl8.6/TclCmd/proc.htm).



## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

## Running the tests
To run the test suite, execute one of the following commands:

```bash
tclsh list-ops.test            # Will stop testing after the first failure.
RUN_ALL=1 tclsh list-ops.test  # Will run all tests and report all failures.
```

## Feedback, Issues, Pull Requests
The [exercism/tcl](https://github.com/exercism/tcl) repository on GitHub is
the home for all of the Tcl exercises on Exercism.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Source

