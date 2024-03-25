# Lucian's Luscious Lasagna

Welcome to Lucian's Luscious Lasagna on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Basics 1

### Constants and functions

A constant value is defined with `name = expression`,
where in Elm, everything except definitions are expressions.

```elm
five = 5

six = 3 + 3
```

Functions are defined with `name parameters... = expression`,
parameters being only separated by space.

```elm
add number1 number2 = number1 + number2
```

Invoking a function also is an expression and is done by
specifying its name and passing arguments for each of the function's parameters,
separated by space, just like for function definition.

```elm
five = add 2 3
```

Parentheses can be used to specify the order of evaluation of an expression.

```elm
six = add 2 (1 * 4)

twelve = add 2 1 * 4
```

### Indentation / significant whitespace

Elm doesn't use syntactic markers such as curly brackets, parentheses, or semicolons to specify code boundaries. It uses whitespaces and indentations instead.

- Module, import, and top-level function definitions must start at the left most column.
- If an expression is split into multiple lines, the code that is part of that expression must be indented under that expression with at least one space.
- Parts of the expression that are grouped together should be indented with equal number of spaces.

```elm
-- A function split over multiple lines, so subsequent lines must be indented
add number1 number2 =
    number1 + number2
```

https://elmprogramming.com/indentation.html

### Modules

Each file in Elm is a module, and must contain a `module` statement before all other code.
Module names must match their file name, so module `Calculator` must be in file Calculator.elm.
Anything defined within a module is privately scoped to it
and cannot be accessed from outside this module, unless listed in `exposing`.

```elm
-- Define the Calculator module, and expose the `add` function
module Calculator exposing (add)

six = 3 + 3

add number1 number2 = number1 + number2
```

```elm
-- Define the Calculator module, and expose everything within: `six` and `add`
module Calculator exposing (..)

six = 3 + 3

add number1 number2 = number1 + number2
```

https://elm-lang.org/docs/syntax#modules

### Comments

A comment is some text within the Elm file that is not interpreted as code.
It is mainly intented to be read by yourself and other programmers.
There is a lightweight syntax for single line comments, based on double dashes.
Multiline comments are also possible with the `{-` and `-}` pair
of opening and closing delimiters.

```elm
-- a single line comment
-- another single line comment

{- a multiline comment
   spawning multiple lines
-}
```

### Formatting

There is a [style guide](https://elm-lang.org/docs/style-guide),
and [elm-format](https://github.com/avh4/elm-format) can be used to automatically format code.

## Instructions

In this exercise you're going to write some code to help you cook a brilliant lasagna from your favorite cookbook.

You have three tasks, all related to the time spent cooking the lasagna.

## 1. Define the expected oven time in minutes

Define `expectedMinutesInOven` to calculate how many minutes the lasagna should be in the oven. According to the cookbook, the expected oven time in minutes is 40:

```elm
expectedMinutesInOven
    --> 40
```

## 2. Calculate the preparation time in minutes

Define `preparationTimeInMinutes` that takes the number of layers in the lasagna as a parameter and returns how many minutes it takes to prepare the lasagna, assuming each layer takes 2 minutes to prepare.

```elm
preparationTimeInMinutes 3
    --> 6
```

## 3. Calculate the elapsed time in minutes

Define the `elapsedTimeInMinutes` function that takes two parameters: the first parameter is the number of layers in the lasagna, and the second parameter is the number of minutes the lasagna has been in the oven. The function should return how many minutes you've worked on cooking the lasagna, which is the sum of the preparation time in minutes and the time in minutes the lasagna has spent in the oven at the moment.

```elm
elapsedTimeInMinutes 3 20
    --> 26
```

## Source

### Created by

- @mpizenberg
- @ceddlyburge