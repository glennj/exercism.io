# Flower Field

Welcome to Flower Field on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

[Flower Field][history] is a compassionate reimagining of the popular game Minesweeper.
The object of the game is to find all the flowers in the garden using numeric hints that indicate how many flowers are directly adjacent (horizontally, vertically, diagonally) to a square.
"Flower Field" shipped in regional versions of Microsoft Windows in Italy, Germany, South Korea, Japan and Taiwan.

[history]: https://web.archive.org/web/20020409051321fw_/http://rcm.usr.dsi.unimi.it/rcmweb/fnm/

## Instructions

Your task is to add flower counts to empty squares in a completed Flower Field garden.
The garden itself is a rectangle board composed of squares that are either empty (`' '`) or a flower (`'*'`).

For each empty square, count the number of flowers adjacent to it (horizontally, vertically, diagonally).
If the empty square has no adjacent flowers, leave it empty.
Otherwise replace it with the count of adjacent flowers.

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

## Additional jq command line options

You'll notice extra options for `jq` in the tests.

```sh
run jq -s -R -c -f flower-field.jq << 'END_INPUT'
```

What are they?

* `-s`

  This is "slurp" mode.
  Instead of handling the input as a stream of JSON objects, 
  all the input will be slurped into an array.

  ```sh
  $ seq 5 | jq -c '.'
  1
  2
  3
  4
  5
  $ seq 5 | jq -s -c '.'
  [1,2,3,4,5]
  ```

* `-R`

  You may have seen `-r` already, to output "raw" strings (without quotes).
  This is the opposite.
  Each line of input is taken to be a string.

  Two examples:

  ```sh
  $ seq 5 | jq -R -c .
  "1"
  "2"
  "3"
  "4"
  "5"

  $ echo 42 | jq '. * 2'      # input is a JSON number
  84
  $ echo 42 | jq -R '. * 2'   # input is a JSON string
  "4242"
  ```

When you put these two options together, you _don't_ get an array of strings:
you get the entire input as a single string, including end-of-line newlines.

```sh
$ seq 5 | jq -s -R -c .
"1\n2\n3\n4\n5\n" 
```

Handling this is part of the challenge of this exercise.

## Source

### Created by

- @glennj
- @IsaacG