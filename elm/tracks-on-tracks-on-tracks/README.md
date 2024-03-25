# Tracks on Tracks on Tracks

Welcome to Tracks on Tracks on Tracks on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Lists

A [`List`][lists] in Elm is an immutable collection of zero or more values of the same type.

Lists can be defined as follows:

```elm
empty = []
singleValue = [ 5 ]
threeValues = [ "a", "b", "c" ]
```

The most common way to add an element to a list is through the `::` (cons) operator:

```elm
twoToFour = [ 2, 3, 4 ]
oneToFour = 1 :: twoToFour --> [ 1, 2, 3, 4 ]
```

Lists are manipulated by functions and operators defined in the [`List` module][list-module] or by [pattern matching][list-destructuring]

```elm
describe : List String -> String
describe list =
    case list of
        [] ->
            "Empty list"

        [ "hello" ] ->
            "Singleton list with: hello"

        x :: xs ->
            "Non-empty list with head: " ++ x

describe []                   --> "Empty list"
describe [ "hello" ]          --> "Singleton list with: hello"
describe [ "hello", "world" ] --> "Non-empty list with head: hello"
```

[lists]: https://elmprogramming.com/list.html
[list-module]: https://package.elm-lang.org/packages/elm/core/latest/List
[list-destructuring]: https://www.bekk.christmas/post/2020/8/peeking-inside-lists

## Instructions

In this exercise you'll be writing code to keep track of a list of programming languages you want to learn on Exercism.
You have six tasks, which will all involve dealing with lists.

## 1. Create a new list

To keep track of the languages you want to learn, you'll need to create a new list.
Define the `newList` constant that contains a new, empty list.

```elm
newList
    --> []
```

## 2. Define an existing list

Currently, you have a piece of paper listing the languages you want to learn: Elm, Clojure and Haskell.
Define the `existingList` constant to represent this list.

```elm
existingList
    --> [ "Elm", "Clojure", "Haskell" ]
```

## 3. Add a new language to a list

As you explore Exercism and find more interesting languages, you want to add them to your list.
Implement the `addLanguage` function to add a new language to the beginning of your list.

```elm
addLanguage "TypeScript" [ "JavaScript", "CoffeeScript" ]
    --> [ "TypeScript", "JavaScript", "CoffeeScript" ]
```

## 4. Count the languages in the list

Counting the languages manually is inconvenient.
Implement the `countLanguages` function to count the number of languages on your list.

```elm
countLanguages [ "C#", "Racket", "Rust", "Ruby" ]
    --> 4
```

## 5. Reverse the list

At some point, you realize that your list is actually ordered backwards!
Implement the `reverseList` function to reverse your list.

```elm
reverseList [ "Prolog", "C", "Idris", "Assembly" ]
    --> [ "Assembly", "Idris", "C", "Prolog" ]
```

## 6. Check if list is exciting

While you love all languages, Elm has a special place in your heart.
As such, you're really excited about a list of languages if:

- The first item on the list is Elm.
- Or the second item on the list is Elm and the list contains three or less languages.

Implement the `excitingList` function to check if a list of languages is exciting:

```elm
excitingList [ "Nim", "Elm" ]
    --> True
```

## Source

### Created by

- @ceddlyburge

### Contributed to by

- @mpizenberg