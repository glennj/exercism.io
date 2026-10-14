# Grains

Welcome to Grains on Exercism's GDScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

There once was a wise servant who saved the life of a prince.
The king promised to pay whatever the servant could dream up.
Knowing that the king loved chess, the servant told the king he would like to have grains of wheat.
One grain on the first square of a chessboard, with the number of grains doubling on each successive square.

## Instructions

Calculate the number of grains of wheat on a chessboard.

A chessboard has 64 squares.
Square 1 has one grain, square 2 has two grains, square 3 has four grains, and so on, doubling each time.

Write code that calculates:

- the number of grains on a given square
- the total number of grains on the chessboard

## Maximum integer value

GDScript uses a signed 64 bit integer for the `int` data type.
This limits how large of a number can be represented by an `int`.

As such, on this track, the exercise assumes a special 6x9 chessboard with a total of 63 squares, not 64.

## Source

### Created by

- @IsaacG

### Based on

The CodeRanch Cattle Drive, Assignment 6 - https://web.archive.org/web/20240908084142/https://coderanch.com/wiki/718824/Grains