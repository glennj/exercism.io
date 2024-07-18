# Strain

Welcome to Strain on Exercism's Wren Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Implement the `keep` and `discard` operation on collections.
Given a collection and a predicate on the collection's elements, `keep` returns a new collection containing those elements where the predicate is true, while `discard` returns a new collection containing those elements where the predicate is false.

For example, given the collection of numbers:

- 1, 2, 3, 4, 5

And the predicate:

- is the number even?

Then your keep operation should produce:

- 2, 4

While your discard operation should produce:

- 1, 3, 5

Note that the union of keep and discard is all the elements.

The functions may be called `keep` and `discard`, or they may need different names in order to not clash with existing functions or concepts in your language.

## Restrictions

Keep your hands off that filter/reject/whatchamacallit functionality provided by your standard library!
Solve this one yourself using other basic tools instead.

## Implementaton Notes

The test suite passes a Function class instance to the `keep` and `discard` methods as a [block argument][block-argument].
To successfully pass the tests, you'll need to call that function inside your code.
If you're unfamiliar with how to do this, please consult the [Function class documentation][function-class].

[block-argument]: https://wren.io/functions.html#block-arguments
[function-class]: https://wren.io/modules/core/fn.html

## Source

### Created by

- @BNAndras

### Based on

Conversation with James Edward Gray II - http://graysoftinc.com/