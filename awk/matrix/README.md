# Matrix

Welcome to Matrix on Exercism's AWK Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a string representing a matrix of numbers, return the rows and columns of
that matrix.

So given a string with embedded newlines like:

```text
9 8 7
5 3 2
6 6 7
```

representing this matrix:

```text
    1  2  3
  |---------
1 | 9  8  7
2 | 5  3  2
3 | 6  6  7
```

your code should be able to spit out:

- A list of the rows, reading each row left-to-right while moving
  top-to-bottom across the rows,
- A list of the columns, reading each column top-to-bottom while moving
  from left-to-right.

The rows for our example matrix:

- 9, 8, 7
- 5, 3, 2
- 6, 6, 7

And its columns:

- 9, 5, 6
- 8, 3, 6
- 7, 2, 7

## Reading from a file

In this exercise, you'll be reading data from a file, not from the main input stream. 
Read about [`getline`][getline] in the Gnu awk manual,
particularly the `Getline/File` and `Getline/Variable/File` forms.

## More about arrays

A two-dimensional array might be a way to implement this solution.
GNU awk has two ways to represent one:

* [multidimensional arrays][multi]
* [arrays of arrays][a-of-a]

[getline]: https://www.gnu.org/software/gawk/manual/html_node/Getline.html
[multi]: https://www.gnu.org/software/gawk/manual/html_node/Multidimensional.html
[a-of-a]: https://www.gnu.org/software/gawk/manual/html_node/Arrays-of-Arrays.html

## Source

### Created by

- @glennj

### Based on

Warmup to the `saddle-points` warmup. - http://jumpstartlab.com