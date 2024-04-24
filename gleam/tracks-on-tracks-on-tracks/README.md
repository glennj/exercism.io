# Tracks on Tracks on Tracks

Welcome to Tracks on Tracks on Tracks on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Lists

A list in Gleam is an immutable collection of zero or more values. The values in a list must all have the same type. As lists are immutable, once a list has been constructed, its value can never change. Any functions/operators that appear to modify a list (such as adding an element), will actually return a new list.

Lists can be defined as follows:

```gleam
let empty = []
let singleValue = [5]
let threeValues = ["a", "b", "c"]
```

The most common way to add an element to a list is through the spread syntax:

```gleam
let two_to_four = [2, 3, 4]
let one_to_four = [1, ..two_to_four]
// -> [1, 2, 3, 4]
```

The [`gleam/list`](https://hexdocs.pm/gleam_stdlib/gleam/list.html) module in the Gleam standard library contains many useful functions for working with lists. This module is very commonly used in Gleam code so it is good to be familiar with it.

Lists patterns can be used in case expressions to match on lists and assign contained values to variables:

```gleam
pub fn describe(list: List(String)) -> String {
  case list {
    [] -> "Empty list"
    [x] -> "List with one item: " <> x
    [x, y] -> "List with two items: " <> x <> " and " <> y
    _ -> "List with three or more items"
  }
}
```

As well as matching on exact length lists, the spread syntax can be used to match on lists of at-least a certain length:

```gleam
pub fn describe(list: List(String)) -> String {
  case list {
    [_, _, ..] -> "List with at least two items"
    [_] -> "List with one item"
    [] -> "Empty list"
  }
}
```

The spread syntax can also be used to assign the rest of the list to a variable:

```gleam
pub fn remove_first_item(list: List(String)) -> List(String) {
  case list {
    // Return the list without the first item
    [_, ..rest] -> rest

    // There's no first item to remove, return an empty list
    _ -> []
  }
}
```

Case expressions should have a pattern for every possible value of the type being matched on, so a final discard pattern (`_`) is often used to handle any remaining values.

## Instructions

In this exercise you'll be writing code to keep track of a list of programming languages you want to learn on Exercism.

You have six tasks, which will all involve dealing with lists.

## 1. Create a new list

To keep track of the languages you want to learn, you'll need to create a new list. Define the `new_list` function that returns a new, empty list.

```gleam
new_list()
// -> []
```

## 2. Define an existing list

Currently, you have a piece of paper listing the languages you want to learn: Gleam, Go, and TypeScript. Define the `existing_list` function to return this list.

```gleam
existing_list()
// -> ["Gleam", "Go", "TypeScript"]
```

## 3. Add a new language to a list

As you explore Exercism and find more interesting languages, you want to add them to your list. Implement the `add_language` function to add a new language to the beginning of your list.

```gleam
add_language(["OCaml", "Elixir"], "Scheme")
// -> ["Scheme", "OCaml", "Elixir"]
```

## 4. Count the languages in the list

Counting the languages one-by-one is inconvenient. Implement the `count_languages` function to count the number of languages on your list.

```gleam
count_languages(["jq", "Elm", "Rust", "Kotlin"])
// -> 4
```

## 5. Reverse the list

At some point, you realize that your list is actually ordered backwards! Implement the `reverse_list` function to reverse your list.

```gleam
reverse_list(["Python", "Julia", "Idris", "COBOL"])
// -> ["COBOL", "Idris", "Julia", "Python"]
```

## 6. Check if list is exciting

While you love all languages, Gleam has a special place in your heart. As such, you're really excited about a list of languages if:

- The first on the list is Gleam.
- The second item on the list is Gleam and the list contain either two or three languages.

Implement the `exciting_list` function to check if a list of languages is exciting:

```gleam
exciting_list(["Lua", "Gleam"])
// -> True
```

## Source

### Created by

- @lpil