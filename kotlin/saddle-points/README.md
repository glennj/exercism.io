# Saddle Points

Welcome to Saddle Points on Exercism's Kotlin Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

You are planning on building a tree house in the woods near your house so that you can watch the sun rise and set.

You've obtained data from a local survey company that shows the heights of all the trees in each rectangular section of the map.
You need to analyze each grid on the map to find the perfect tree for your tree house.

The best tree will be the tallest tree compared to all the other trees to the east and west, so that you have the best possible view of the sunrises and sunsets.
You don't like climbing too much, so the perfect tree will also be the shortest among all the trees to the north and to the south.

## Instructions

Your task is to find the potential trees where you could build your tree house.

The data company provides the data as grids that show the heights of the trees.
The rows of the grid represent the east-west direction, and the columns represent the north-south direction.

An acceptable tree will be the the largest in its row, while being the smallest in its column.

A grid might not have any good trees at all.
Or it might have one, or even several.

Here is a grid that has exactly one candidate tree.

    1  2  3  4
  |-----------
1 | 9  8  7  8
2 | 5  3  2  4  <--- potential tree house at row 2, column 1, for tree with height 5
3 | 6  6  7  1

- Row 2 has values 5, 3, and 1. The largest value is 5.
- Column 1 has values 9, 5, and 6. The smallest value is 5.

So the point at `[2, 1]` (row: 2, column: 1) is a great spot for a tree house.

## Source

### Created by

- @stkent

### Contributed to by

- @dector
- @eparovyshnaya
- @lihofm
- @mdowds
- @sjwarner-bp
- @SleeplessByte
- @uzilan

### Based on

J Dalbey's Programming Practice problems - http://users.csc.calpoly.edu/~jdalbey/103/Projects/ProgrammingPractice.html