# Queen Attack

Welcome to Queen Attack on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given the position of two queens on a chess board, indicate whether or not they
are positioned so that they can attack each other.

In the game of chess, a queen can attack pieces which are on the same
row, column, or diagonal.

A chessboard can be represented by an 8 by 8 array.

So if you're told the white queen is at (2, 3) and the black queen at
(5, 6), then you'd know you've got a set-up like so:

```text
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ W _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ B _
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
```

You'd also be able to answer whether the queens can attack each other.
In this case, that answer would be yes, they can, because both pieces
share a diagonal.

## Exception messages

Sometimes it is necessary to [raise an exception](https://docs.python.org/3/tutorial/errors.html#raising-exceptions). When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging. For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types](https://docs.python.org/3/library/exceptions.html#base-classes), but should still include a meaningful message.

This particular exercise requires that you use the [raise statement](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) to "throw" `ValueErrors` if the `Queen` constructor is passed numbers that are negative, or numbers that are not a valid row or column.  The tests will only pass if you both `raise` the `exception` and include a message with it. You also should `raise` a `ValueError` if both the queens passed to the `can_attack()` method are on the same location.

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:

```python
# if the row parameter is negative
raise ValueError("row not positive")

# if the row parameter is not on the defined board
raise ValueError("row not on board")

# if the column parameter is negative
raise ValueError("column not positive")

# if the column parameter is not on the defined board
raise ValueError("column not on board")

# if both the queens are on the same location
raise ValueError("Invalid queen position: both queens in the same square")
```

## Source

### Created by

- @betegelse

### Contributed to by

- @beatorizu
- @behrtam
- @BethanyG
- @cmccandless
- @denislooby
- @Dog
- @fortrieb
- @ikhadykin
- @jnaulty
- @kytrinyx
- @N-Parsons
- @olufotebig
- @pheanex
- @sjakobi
- @tqa236
- @xitanggg

### Based on

J Dalbey's Programming Practice problems - http://users.csc.calpoly.edu/~jdalbey/103/Projects/ProgrammingPractice.html