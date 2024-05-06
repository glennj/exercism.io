# Secure Treasure Chest

Welcome to Secure Treasure Chest on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Opaque Types

Opaque types in Gleam are custom types where only the module that defines the type can construct or pattern match values of the type. This is useful for creating types that should only be used in a specific way.

Opaque types are defined using the `opaque` keyword.

```gleam
pub opaque type PositiveInt {
  PositiveInt(inner: Int)
}
```

This `PositiveInt` type is to be used in situations where an int is wanted, but it has to be zero or greater. A regular Gleam `Int` could not be used as they can also be negative.

The module that defines this type can define a function to get the inner int value, and a function for creating the `PositiveInt` type which will return an error if the value is not positive.

```gleam
pub fn from_int(i: Int) -> Result(PositiveInt, String) {
  case i {
    _ if i < 0 -> Error("Value must be positive")
    _ -> Ok(PositiveInt(i))
  }
}

pub fn to_int(i: PositiveInt) -> Int {
  i.inner
}
```

With this API other modules cannot construct a `PositiveInt` with a negative value, so any function that takes a `PositiveInt` can be sure that the value is positive.

## Instructions

Sharp eyed students will have noticed that the `TreasureChest` type in the previous exercise wasn't that secure!

If you used the `get_treasure` function you had to supply the password, but you could still destructure the `TreasureChest` type to get the treasure without having to know the password.

Let's fix that by using an Opaque Type.

## 1. Define the `TreasureChest` opaque type.

The `TreasureChest` contains two fields:
- A password that is a `String`.
- A treasure that is a generic type.

The `TreasureChest` type must be opaque.

## 2. Define the `create` function.

This function takes two arguments:
- A password `String`.
- A treasure value of any type.

The function returns a `TreasureChest` containing the password and the value.

If the password is shorter than 8 characters then the function should return an error saying `Password must be at least 8 characters long`.

## 3. Define `open` function.

This function takes two arguments:

- A `TreasureChest`.
- A password `String`.

If the password matches the password in the `TreasureChest` then the function should return the treasure, otherwise it should return an error saying `Incorrect password`.

## Source

### Created by

- @lpil