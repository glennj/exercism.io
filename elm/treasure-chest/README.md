# Treasure Chest

Welcome to Treasure Chest on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Generics

### Introduction

Generics are a language feature that enable code reuse.
For example, it should not matter what type a list holds in order to compute its length.
That is why the `length` function uses a generic type parameter `a` in its type signature.

```elm
length : List a -> Int
```

### Parameters in types

When a type is generic over another type, we use a lowercase name for that other type, and call it a type parameter.
Common examples are the `List`, `Maybe` and `Result` custom types.

```elm
-- Maybe is generic over the type a
type Maybe a
    = Nothing
    | Just a

someString : Maybe String
someString = Just "hello"

someInt : Maybe Int
someInt = Just 42
```

One important remark about names of type parameters, is that they can be multiple characters long.
The only constraint is that they must be lowercase names.
As such, when useful, we tend to give them descriptive names.

A type can be generic over multiple other types, and thus have multiple type parameters.
In that case, every independent type gets a different lowercase name.
Being independent does not prevent them from becoming the same type, it just enables them to be different.

```elm
-- Result is generic over the types error and value
type Result error value
    = Ok value
    | Err error
```

Record type aliases can also have type parameters, although it is less common.

```elm
-- Event is generic over the type a
type alias Event a =
    { at : Time
    , data : a 
    }
```

### Type parameters in functions

Since types can be generic, so can functions.
Functions handling generic types can be writen, such as the `Maybe.withDefault`.

```elm
withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
      Just value -> value
      Nothing -> default
```

Functions that take other functions as parameters are called higher order functions, and these can also be generic.

```elm
map : (a -> b) -> Maybe a -> Maybe b
map function maybe =
  case maybe of
    Just value ->
      Just (function value)

    Nothing ->
      Nothing
```

## Instructions

In this exercise you're going to write a generic (/ magical!) TreasureChest, to store some treasure.

## 1. Define the TreasureChest generic custom type

Define a `TreasureChest` custom type with a single variant.

The variant should have an associated `String` value, which will be used to store the password of the treasure chest.

The variant should also have an associate generic value, which will be used to store the treasure.
This value is generic so that the treasure can be anything.

## 2. Define the getTreasure function

This function should take two parameters

- a `String` (for trying a password)
- a `TreasureChest` generic custom type

This function should check the provided password attempt against the `String` in the `TreasureChest`.

The function should return a `Maybe`.

If the passwords match then return `Just` with the generic value from the `TreasureChest` (the treasure!)

If the passwords do not match then return `Nothing`.

## 3. Define the multiplyTreasure function

This (higher order) function should take two parameters

- a multiplier function that takes one generic parameter, and returns a list of the same generic type
- a `TreasureChest` generic custom type, also with the same generic type

This function should call the multiplier function with the treasure in the chest.

The function should return a new TreasureChest, with the same password as the original, and with the multiplied treasure (the return)

## Source

### Created by

- @ceddlyburge
- @mpizenberg
- @jiegillet