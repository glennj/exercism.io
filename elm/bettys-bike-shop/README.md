# Betty's Bike Shop

Welcome to Betty's Bike Shop on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Basics 2

### Exporting functions

Exporting functions was covered in the first concept, here is a quick refresher.

Each file in Elm is a module and must contain a `module` statement before all other code.

Module names must match their file name, so module `Calculator` must be in file Calculator.elm, and module `Parser.Utils` needs to be in file Parser/Utils.elm.

Anything defined within a module is privately scoped to it and cannot be accessed from outside this module, unless listed in `exposing`.

```elm
-- Calculator.elm
-- Define the Calculator module, and expose the `add` function
module Calculator exposing (add)

six = 3 + 3

add number1 number2 = number1 + number2
```

```elm
-- Calculator.elm
-- Define the Calculator module, and expose everything within: `six` and `add`
module Calculator exposing (..)

six = 3 + 3

add number1 number2 = number1 + number2
```

https://elm-lang.org/docs/syntax#modules

### Importing functions from other modules

Accessing functions defined in other modules is done via imports.
All functions within that module that were exposed by it are made accessible when importing that module.
But how they are accessed varies depending on how the module is imported.

Qualified imports are the default, and accessing a function within such module (for example the `map` function in the `List` module) is done by prefixing the module name (`List.map`).

Open imports enable direct access to the exposed functions within that module, without prefixing. They are done with the `exposing` keyword, like a mirror to the one used in exports. You can expose all available functions (with `..`), or specific ones (for example `map`).

You can also import types, and their constructors from other modules, which is a later concept.

Qualified imports are preferred to aid clarity and avoid name clashes.

```elm
-- qualified imports
import List                            -- List.map, List.foldl
import List as L                       -- L.map, L.foldl

-- open imports
import List exposing (..)              -- map, foldl, concat, ...
import List exposing ( map, foldl )    -- map, foldl, List.concat
```

https://elm-lang.org/docs/syntax#modules

### Type annotations

Type annotations are defined with `name : parameter types -> return type`, parameter types also being separated by `->`.

```elm
add : Int -> Int -> Int
add number1 number2 = number1 + number2
```

Parentheses can be used to define function parameters (which are themselves defined with the same syntax, hence the need for brackets).

```elm
-- Transform every character in a string
map : (Char -> Char) -> String -> String
map charMapperFunction string =
    -- ...
```

https://elm-lang.org/docs/syntax#type-annotations

### Numbers

There are two types of numbers available in Elm, defined by the `Int` and `Float` types.
`Int` corresponds to the set of positive and negative integers.
`Float` corresponds to the set of real numbers, limited by the precision of the computer.
Operations defined on numbers usually work on one type or the other, but not mixing them.
There exists functions to convert between the two, such that `toFloat` which converts `Int` to `Float` and `round`, `floor`, `ceiling` or `truncate` which convert a `Float` to an `Int` with different semantics.

https://package.elm-lang.org/packages/elm/core/latest/Basics#Int

### String operations

To describe words and sentences in code, Elm has a native `String` type.
String literals are written between double quotes such as `"Hello World!"`.
A string concatenation operator `++` and equality operator `==` are available by default.

```elm
hello : String -> String
hello subject = "Hello " ++ subject

hello "World!" --> "Hello World!"
```

Most other string functions are in the `String` module, which needs to be imported.

https://package.elm-lang.org/packages/elm/core/latest/String

## Instructions

In this exercise you're going to write some code to help Betty's Bike Shop, an online shop for bikes and parts.
You have three tasks, aiming at correctly displaying the bikes and parts prices on the website.

## 1. Export the `penceToPounds` function

Your colleague has already written the skeleton of the module, the undefined functions `penceToPounds` and `poundsToString`.
However, they have forgotten to export the `penceToPounds` function, so it is currently not visible by other modules or by the test suite.

Add `penceToPounds` to the list of exported functions.

## 2. Convert pence to pounds

Currently, the price is stored as an integer number of *pence* (the bike shop is based in the UK).
On the website, we want to show the price in *pounds*, where 1.00 pound amounts to 100 pence.
Your first task is to implement the `penceToPounds` function, taking an `Int` amount of pence, and converting it into its equivalent pounds as a `Float`.
You should also define the type annotation for that function.

```elm
penceToPounds 106
    --> 1.06
```

## 3. Format the price for display on the website

Since Betty's bikes are sold in pounds, prices should be displayed with the symbol "£".
Your second task is thus to implement the `poundsToString` function, taking an amount of pounds as a `Float` and returning its price displayed as a `String` with the pound symbol prepended.
The `String` module contains the `String.fromFloat` function that will be helpful for this.
You should import the `String` module before using it.
You should also define the type annotation for `poundsToString`.

```elm
poundsToString 1.06
    --> "£1.06"
```

## Source

### Created by

- @ceddlyburge
- @mpizenberg

### Contributed to by

- @jiegillet