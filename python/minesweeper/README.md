# Minesweeper

Welcome to Minesweeper on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

[Minesweeper][wikipedia] is a popular game where the user has to find the mines using numeric hints that indicate how many mines are directly adjacent (horizontally, vertically, diagonally) to a square.

[wikipedia]: https://en.wikipedia.org/wiki/Minesweeper_(video_game)

## Instructions

Your task is to add the mine counts to empty squares in a completed Minesweeper board.
The board itself is a rectangle composed of squares that are either empty (`' '`) or a mine (`'*'`).

For each empty square, count the number of mines adjacent to it (horizontally, vertically, diagonally).
If the empty square has no adjacent mines, leave it empty.
Otherwise replace it with the adjacent mines count.

For example, you may receive a 5 x 4 board like this (empty spaces are represented here with the '·' character for display on screen):

```text
·*·*·
··*··
··*··
·····
```

Which your code should transform into this:

```text
1*3*1
13*31
·2*2·
·111·
```

## Exception messages

Sometimes it is necessary to [raise an exception](https://docs.python.org/3/tutorial/errors.html#raising-exceptions). When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging. For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types](https://docs.python.org/3/library/exceptions.html#base-classes), but should still include a meaningful message.

This particular exercise requires that you use the [raise statement](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) to "throw" a `ValueError` when the `board()` function receives malformed input. The tests will only pass if you both `raise` the `exception` and include a message with it.

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:

```python
# when the board receives malformed input
raise ValueError("The board is invalid with current input.")
```

## Source

### Created by

- @betegelse

### Contributed to by

- @alexpjohnson
- @behrtam
- @BethanyG
- @cmccandless
- @Dog
- @fluxusfrequency
- @ikhadykin
- @kytrinyx
- @N-Parsons
- @peterblazejewicz
- @pheanex
- @sjakobi
- @smalley
- @tqa236