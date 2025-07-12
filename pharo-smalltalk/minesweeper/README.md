# Minesweeper

Welcome to Minesweeper on Exercism's Pharo Track.
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

x,y locations are often represented as a Point, which then leads to some useful point functions that can help with this. Alternatively, representing this as a 2D Array can also be useful. Either way,  defining some descriptive extension methods that can make your solution much more readable and elegant.

## Source

### Created by

- @macta