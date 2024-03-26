# Squeaky Clean

Welcome to Squeaky Clean on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Strings

Characters and Strings form the basis for text representation in Elm.
Strings can be thought of as lists of characters and most `List` functions have their equivalent in the `String` module.

### Characters

The `Char` type represents a single unicode character with single quotes e.g. `'a'`. 
The `Char` module provides predicate functions (`Char -> Bool`) to test qualities of a character: `isUpper`, `isLower`, `isAlpha`, and `isAlphaNum`
The module also provides functions to convert a character `toUpper`, `toLower`, `toLocaleUpper`, `toLocaleLower`, as well as to/from their numerical unicode value (`Int`) with `toCode` and `fromCode`.

### Strings

The `String` type provides a built-in representation for efficient string manipulation and can represent any sequence of unicode characters e.g. "this is an Elm string."
Multi-line strings are represented with triple double quotes and can have unescaped quotes and newlines.
```elm
"""
    This is a multiline string.
All line-breaks and whitespace,
are preserved. 
"""
```

### String Functions

The `String` module contains all the functions summarized on the table below.
Some of the most commonly used functions include:

`cons : Char -> String -> String` adds a character to the beginning of a string.
```elm
cons 'T' "he truth is out there" 
    --> "The truth is out there"
```

`(++): String -> String -> String` appends two strings.
```elm
"butter" ++ "fly" 
    --> "butterfly"
```

`split : String -> String -> List String` splits a string into a list of strings at the given separator.
```elm
split ";" "cat;dog;cow" 
    --> ["cat", "dog", "cow"]
```

`join : String -> List String -> String` concatenates a list of strings together with a given separator.
```elm
join "a" ["H", "w", "ii", "n"] 
    --> "Hawaiian"
```
`replace : String -> String -> String -> String` replaces all occurrences of a given substring.
```elm
replace "Hello" "Goodbye cruel" "Hello world." 
    --> "Goodbye cruel world."
```

`length : String -> Int` returns the number of characters in a string.
```elm 
length "Hello world." 
    --> 12
```

`toInt : String -> Maybe Int` tries to convert a string into `Just Int`, and returns `Nothing` for improperly formatted strings.
```elm
toInt "-42" 
    --> Just -42
```    

`contains : String -> String -> Bool` checks if the second string contains the first one.
```elm
contains "the" "theory" 
    --> True
```

`map : (Char -> Char) -> String -> String` applies a given function to every `Char` in the string.
Note the function provided must be a mapping from and to `Char`.
```elm
map (\c -> if c == '/' then '.' else c) "a/b/c" 
    --> "a.b.c"
```

### All String Functions:
| Manipulate | Substrings | Check       | Convert      | Higher Order |
|------------|------------|-------------|--------------|--------------|
| `reverse`  | `slice`    | `length`    | `toInt`      | `map`        | 
| `repeat`   | `left`     | `isEmpty`   | `fromInt`    | `filter`     | 
| `replace`  | `right`    | `contains`  | `toFloat`    | `foldl`      |
| `append`   | `dropLeft` | `startsWith`| `fromFloat`  | `foldr`      |   
| `concat`   | `dropRight`| `indexes`   | `toList`     | `any`        |
| `split`    |            |             | `fromList`   | `all`        |
| `join`     |            |             | `fromChar`   |              |
| `words`    |            |             |              |              |
| `lines`    |            |             |              |              |
| `cons`     |            |             |              |              |
| `uncons`   |            |             |              |              |

## Instructions

In this exercise you will implement a partial set of utility functions to help a developer clean up identifier names.

In the 5 tasks you will gradually build up the `clean` function.
A valid identifier comprises zero or more letters and underscores.

If an empty string is passed to the `clean` function, an empty string should be returned.

## 1. Replace spaces with underscores

Implement a `clean1` function to replace any spaces with underscores. This also applies to leading and trailing spaces.

```elm
clean1 "my   Id"
    --> "my___Id"
```

## 2. Replace control characters with the upper case string "[CTRL]"

Implement `clean2` to do everything the previous function does and replace common control characters (\n, \t, and \r ) with the string `"[CTRL]"`.

```elm
clean2 "my\nId"
    --> "my[CTRL]Id",
```

## 3. Convert kebab-case to camelCase

Implement `clean3` to do everything the previous function does and convert kebab-case to camelCase.

```elm
clean3 "à-ḃç"
    --> "àḂç"
```

## 4. Omit digits 

Implement 'clean4` to do everything the previous function does and omit all digits.

```elm
clean4 "123"
    --> ""
```

## 5. Omit Greek lower case letters

Finally, implement `clean` to do everything the previous function does and omit any Greek letters in the range 'α' to 'ω'.

```elm
clean "My Οβιεγτ Finder"
    --> "My_Ο_Finder"
```

## Source

### Created by

- @pwadsworth

### Contributed to by

- @jiegillet
- @ceddlyburge