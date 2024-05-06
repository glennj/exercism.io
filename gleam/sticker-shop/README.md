# Sticker Shop

Welcome to Sticker Shop on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Phantom Types

Phantom types are type parameters of a custom type that are not used in any of the value constructors of that type.

That's a little abstract, so here is an example:

```gleam
pub type Length(unit) {
  Length(amount: Float)
}
```

In this example the `unit` type parameter is not used in the `Length` value constructor, so `unit` is a phantom type.

This unused type parameter may seem useless, but it can be used to add further restrictions on how `Length` values can be used.

For example, we could have a function `double`, which multiplies the length. It works with lengths of any unit, so the type parameter is a generic type variable.

```gleam
// This function accepts all Length values
pub fn double(length: Length(unit)) -> Length(unit) {
  Length(length.amount *. 2.0)
}
```

We could also have a function `add_inch`, which only works if the length is in inches.

```gleam
// A unit type for inches. It is never constructed so we don't
// define any constructors for it.
pub type Inches

pub fn add_inch(length: Length(Inches)) -> Length(Inches) {
  Length(length.amount +. 1.0)
}
```

The `add_inch` function will not accept lengths of any other unit parameter, the phantom type has been used to ensure only the correct unit is used.

A function can also be written to ensure that two length values are of the same unit, by using the same type variable for both.

```gleam
pub fn add(a: Length(unit), b: Length(unit)) -> Length(unit) {
  Length(a.amount +. b.amount)
}
```
```gleam
let two_meters: Length(Meters) = Length(2.0)
let two_inches: Length(Inches) = Length(2.0)

add(two_meters, two_meters)
// -> Length(4.0): Length(Meters)

add(two_meters, two_inches)
// Type error! The unit type parameters do not match.
```

Phantom types can work well with opaque types. If other modules cannot construct `Length` values then we can ensure they are not constructed with an invalid unit type, and that only the functions defined above can be used with them.

## Instructions

Lucy has an online sticker shop, where she sells cute stickers featuring everyone's favourite programming languages. People from all around the world buy her stickers, and she's having some trouble dealing with all the different currencies.

Create a program that Lucy can use to calculate prices while being sure that she's always using the correct currency.

## 1. Define the `Usd`, `Eur`, and `Jpy` types

These types are used to represent the different currencies that Lucy's customers use to buy her stickers.

They are to be used as phantom types and do not need to have any constructors.

## 2. Define the `Money` type.

The `Money` type should have an `Int` field for the amount of money, a currency phantom type parameter, and it should be an opaque type.

## 3. Define `dollar`, `euro`, and `yen` functions

Define the `dollar`, `euro`, and `yen` functions that take an `Int` argument and return a `Money` value with the correct currency.

## 4. Define the `total` function

Define the `total` function which takes a list of `Money` values and returns the total amount of money in the list.

```gleam
total([euro(120), euro(200), euro(145)])
// -> euro(465)
```

## Source

### Created by

- @lpil