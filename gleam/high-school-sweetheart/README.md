# High School Sweetheart

Welcome to High School Sweetheart on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Pipe Operator

The `|>` operator is called the pipe operator. It can be used to chain function calls together in such a way that the value returned by the previous function call is passed to the next function call.

```gleam
"hello"
|> string.uppercase
|> string.append("?!")
// -> "HELLO?!"
```

The above code is equivalent to the following:

```gleam
string.append(string.uppercase("hello"), "?!")
```

The pipe operator will either pass the value as the first argument to the function call, or the only argument to a new call, selecting whichever would have the correct type.

```gleam
100
|> function_that_takes_two_arguments(1)

// Is equivalent to
function_that_takes_two_arguments(100, 1)
```

```gleam
100
|> function_that_returns_a_function(1)

// Is equivalent to
function_that_returns_a_function(1)(100)
```

Sometimes we want to pass the value into another position, in this case the `_` placeholder can be used to indicate where the value should be inserted.

```gleam
100
|> some_function(1, _, 2)
```

## Instructions

In this exercise, you are going to help high school sweethearts profess their love on social media by generating an ASCII heart with their initials:

```
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     J. K.  +  M. B.     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
```

## 1. Get the name's first letter

Implement the `first_letter` function. It should take a name and return its first letter. It should clean up any unnecessary whitespace from the name.

```gleam
first_letter("Jane")
// -> "J"
```

## 2. Format the first letter as an initial

Implement the `initial` function. It should take a name and return its first letter, uppercase, followed by a dot. Make sure to reuse `first_letter` that you defined in the previous step.

```gleam
initial("Robert")
// -> "R."
```

## 3. Split the full name into the first name and the last name

Implement the `initials` function. It should take a full name, consisting of a first name and a last name separated by a space, and return the initials. Make sure to reuse `initial` that you defined in the previous step.

```gleam
initials("Lance Green")
// -> "L. G."
```

## 4. Put the initials inside of the heart

Implement the `pair` function. It should take two full names and return the initials inside an ASCII heart. Make sure to reuse `initials` that you defined in the previous step.

```gleam
pair("Blake Miller", "Riley Lewis")
// -> "
//      ******       ******
//    **      **   **      **
//  **         ** **         **
// **            *            **
// **                         **
// **     B. M.  +  R. L.     **
//  **                       **
//    **                   **
//      **               **
//        **           **
//          **       **
//            **   **
//              ***
//               *
// "
```

## Source

### Created by

- @lpil