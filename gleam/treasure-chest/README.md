# Treasure Chest

Welcome to Treasure Chest on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Generics

A type is a _generic type_ when it can contain values of any type using a _type parameter_.

For example, this `Box` type has the type parameter `a` (written with lowercase letters).

```gleam
pub type Box(a) {
  Box(a)
}
```

The `a` type parameter is a placeholder for any type, so `Box` can be used with strings, ints, or any other type.

```gleam
Box("Hello, Joe!") // The type is Box(String)

Box(42) // The type is Box(Int)
```

A type can have multiple type parameters by separating them with commas.

```gleam
pub type Pair(a, b) {
  Pair(a, b)
}
```

### Generic functions

Type parameters can also be used in the arguments of functions.

This function takes a value of the type `a` and calls twice a function that takes an `a` and returns a `b`.

```gleam
pub fn twice(value: a, f: fn(a) -> b) -> b {
  f(value)
  f(value)
}
```
```gleam
// The type is fn(String, fn(String) -> Nil) -> Nil
twice("Hello, Joe!", io.println)
```

## Instructions

In this exercise you're going to write a generic (/ magical!) TreasureChest, to store some treasure.

## 1. Define the TreasureChest generic custom type

Define a `TreasureChest` custom type with a single variant.

The variant should have an associated `String` value, which will be used to store the password of the treasure chest.

The variant should also have an associate generic value, which will be used to store the treasure.
This value is generic so that the treasure can be anything.

## 2. Define the UnlockResult type

Define a `UnlockResult` custom type with two variants.

The first variant should be `Unlocked`, and should hold generic value, which will be used to store the treasure.

The second variant should be `WrongPassword`, which holds no values.

## 3. Define the get_treasure function

This function should take two parameters

- a `TreasureChest` generic custom type
- a `String` (for trying a password)

This function should check the provided password attempt against the `String` in the `TreasureChest`.

The function should return a `UnlockResult`.

If the passwords match then return `Unlocked` with the generic value from the `TreasureChest` (the treasure!)

If the passwords do not match then return `WrongPassword`.

## Source

### Created by

- @lpil