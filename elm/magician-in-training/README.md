# Magician in training

Welcome to Magician in training on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Arrays

An [`Array`][array] in Elm is a fast immutable collection of zero or more values of the same type.

Unlike `List` which is a [default-import][default-imports], the `Array` module must be explicity imported, using `import Array` or similar.

Arrays can be defined as follows:

```elm
import Array exposing (Array)

empty : Array Char
empty = Array.empty

singleValue = Array.fromList [ 5 ] --> Array Int

repeatSingleValue = Array.repeat 2 5 --> Array.fromList [ 5, 5 ]

initialiseFromIndex = Array.initialise 3 (\index -> index * 2) --> Array.fromList [ 0, 2, 4 ]

initialisefromList = Array.fromList [ "a", "b", "c" ] --> Array.fromList [ "a", "b", "c" ]
```

Elements are added to an Array using `Array.push`:

```elm
twoToFour = Array.fromList [ 2, 3, 4 ]
oneToFour = Array.push 1 twoToFour --> Array.fromList [ 1, 2, 3, 4 ]
```

An element in an Array is retrieved using `Array.get`:

```elm
sixSeven = Array.fromList [ 6, 7 ]
Array.get 0 sixSeven --> Just 6
```

An element in an Array is set using `Array.set`:

```elm
sixSeven = Array.fromList [ 6, 7 ]
Array.set 1 8 sixSeven --> Array.fromList [ 6, 8 ]
```

Arrays are manipulated by functions defined in the [`Array` module][array-module].

```elm
oneTwo = Array.fromList [ 1, 2 ]
Array.length oneTwo --> 2
```

[array]: https://elmprogramming.com/array.html
[array-module]: https://package.elm-lang.org/packages/elm/core/latest/Array
[default-imports]: https://github.com/elm/core#default-imports

## Instructions

As a magician-to-be, Elyse needs to practice some basics. She has a deck of cards that she wants to manipulate.

To make things a bit easier she only uses the cards 1 to 10.

## 1. Retrieve a card from a deck

Implement the `getCard` function  that returns the card at position `index` from the given deck. If the index is not in the deck, return Nothing.

```elm
getCard 2, ( Array.fromList [ 1, 2, 4, 1 ] )
    --> Just 4
getCard 6, ( Array.fromList [ 1, 2, 4, 1 ] )
    --> Nothing
```

## 2. Change a card in the deck

Implement the `setCard` function that returns a deck with the card at position `index` changed to the new card provided.
If the `index` is not in the deck, return the original deck.

```elm
setCard 2, 6, ( Array.fromList [ 1, 2, 4, 1 ] )
    --> Array.fromList [ 1, 2, 6, 1 ]
setCard 6, 6, ( Array.fromList [ 1, 2, 4, 1 ] )
    --> Array.fromList [ 1, 2, 4, 1 ]
```

## 3. Add a card to the deck

Implement the `addCard` function that returns a deck with the new card added.

```elm
addCard 8 ( Array.fromList [ 5, 9, 7, 1 ] )
    --> Array.fromList [ 5, 9, 7, 1, 8 ]
```

## 4. Remove a card from the deck

Implement the `removeCard` function that returns a deck with the card at position `index` removed.
If the given `index` is not a valid index in the deck, return the original deck.

```elm
removeCard 2 ( Array.fromList [ 3, 2, 6, 4, 8 ] )
    --> Array.fromList [ 3, 2, 4, 8 ]
removeCard 2 ( Array.fromList [ 3, 2 ] )
    --> Array.fromList [ 3, 2 ]
```

## 5. Count the number of even cards in the deck

Implement the `evenCardCount` function that counts the number of even cards in the deck.

```elm
evenCardCount (Array.fromList [ 3, 8, 4, 5, 1, 6, 10 ] )
    --> 4
```

## Source

### Created by

- @ceddlyburge