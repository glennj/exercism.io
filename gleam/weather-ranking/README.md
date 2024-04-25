# Weather Ranking

Welcome to Weather Ranking on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Constants

Constants in Gleam are a way to give names to values. They are defined using the `const` keyword.

```gleam
pub const pi: Float = 3.14159

pub fn circle_area(radius: Float) -> Float {
  radius *. radius *. pi
}
```

When defined with the `pub` keyword module constants can be accessed from other modules, otherwise they can only be accessed from within the module they are defined in.

## Orders

The `gleam/order` module in Gleam's standard library contains the `Order` type, which is used to compare values.

The definition of `Order` looks like this:

```gleam
pub type Order {
  Gt
  Eq
  Lt
}
```

The type has three variants, each used to represent the relationship between value and another value:

- `Gt` - the value is _larger_ than the other value.
- `Eq` - the value is _equal_ to the other value.
- `Lt` - the value is _smaller_ than the other value.

If a data type can be compared by size it is common for the module to define a `compare` function which returns an `Order` value. For example, `gleam/float` and `gleam/int` define `compare` functions that operate on floats and ints respectively.

```gleam
int.compare(1, 2)
// -> Lt

int.compare(2, 2)
// -> Eq

float.compare(1.0, 2.0)
// -> Lt
```

Sorting functions such as `list.sort` commonly take a function that returns an `Order` value, allowing the caller to define how the list should be sorted.

```gleam
list.sort([3, 7, 3, 1], by: int.compare)
// -> [1, 3, 3, 7]
```

## Instructions

Tori is a climate journalist and she wants to write an article about the weather in different cities. She has data about the weather in different cities and she wants to rank them based on their temperature, but some of the temperatures are in Fahrenheit and some are in Celsius. She needs your help to convert the temperatures to the same unit and rank them.

## 1. Convert Fahrenheit to Celsius

Define the `fahrenheit_to_celsius` function that takes a temperature in Fahrenheit as an argument and returns the temperature in Celsius.

To convert Fahrenheit to Celsius subtract 32 from the Fahrenheit value, and then divide the result by 1.8.

```gleam
fahrenheit_to_celsius(72.5)
// -> 22.5
```

## 2. Compare two temperatures

Define the `compare_temperature` function that takes two temperatures and returns an `Order` value indicating whether the first temperature is less than, equal to, or greater than the second temperature.

```gleam
compare_temperature(Celsius(30.5), Fahrenheit(82.1))
// -> Gt
```

## 3. Sort cities by temperature

Define the `sort_cities_by_temperature` function that takes a list of cities and returns them from coldest to warmest.

```gleam
sort_cities_by_temperature([
  City(name: "London", temperature: Celsius(30.5)),
  City(name: "Paris", temperature: Fahrenheit(82.1))
])
// -> [
//   City(name: "Paris", temperature: Fahrenheit(82.1)),
//   City(name: "London", temperature: Celsius(30.5)))
// ]
```

## Source

### Created by

- @lpil