# Saddle Points

Welcome to Saddle Points on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Detect saddle points in a matrix.

So say you have a matrix like so:

```text
    1  2  3
  |---------
1 | 9  8  7
2 | 5  3  2     <--- saddle point at row 2, column 1, with value 5
3 | 6  6  7
```

It has a saddle point at row 2, column 1.

It's called a "saddle point" because it is greater than or equal to every element in its row and less than or equal to every element in its column.

A matrix may have zero or more saddle points.

Your code should be able to provide the (possibly empty) list of all the saddle points for any given matrix.

The matrix can have a different number of rows and columns (Non square).

Note that you may find other definitions of matrix saddle points online, but the tests for this exercise follow the above unambiguous definition.

## Exception messages

Sometimes it is necessary to [raise an exception](https://docs.python.org/3/tutorial/errors.html#raising-exceptions). When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging. For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types](https://docs.python.org/3/library/exceptions.html#base-classes), but should still include a meaningful message.

This particular exercise requires that you use the [raise statement](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) to "throw" a `ValueError` if the `matrix` is irregular. The tests will only pass if you both `raise` the `exception` and include a message with it.

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:

```python
# if the matrix is irregular
raise ValueError("irregular matrix")
```

## Source

### Created by

- @betegelse

### Contributed to by

- @Alexhans
- @AnAccountForReportingBugs
- @behrtam
- @BethanyG
- @cmccandless
- @crsmi
- @ctholho
- @DocFogel
- @Dog
- @elyssonmr
- @ikhadykin
- @ilmanzo
- @kytrinyx
- @N-Parsons
- @pheanex
- @pywkm
- @sjakobi
- @tqa236
- @yawpitch

### Based on

J Dalbey's Programming Practice problems - https://users.csc.calpoly.edu/~jdalbey/103/Projects/ProgrammingPractice.html