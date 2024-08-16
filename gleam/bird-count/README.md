# Bird Count

Welcome to Bird Count on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Recursion

The ability for something to be defined in terms of itself is called recursion. In Gleam, recursion is most commonly found in recursive functions, which are functions that call themselves.

A recursive function needs to have at least one _base case_ and at least one _recursive case_. A _base case_ returns a value without calling the function again. A _recursive case_ calls the function again, modifying the input so that it will at some point match the base case.

```gleam
pub fn factorial(x: Int) -> Int {
  case x {
    // Base case
    1 -> 1

    // Recursive case
    _ -> x * factorial(x - 1)
  }
}
```

Gleam has no special syntax for looping, so all looping is done with recursion.

```gleam
pub fn list_length(list: List(String)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}
```

Gleam also supports recursive custom types. A recursive custom type has one or more of its variants refer to itself in their contained data.

```gleam
pub type RussianDoll {
  Child               // Base case
  Mother(RussianDoll) // Recursive case
}
```
```gleam
let very_big_doll = Mother(Mother(Mother(Child)))
let small_doll = Mother(Child)
```

## Instructions

Izzy is an avid bird watcher that keeps track of how many birds have visited her garden on any given day.

She's asked you to help bring her bird watching to a new level and implement a few tools that will help her track and process the data.

You have chosen to store the data as a list of integers. The first number in the list is the number of birds that visited your garden today, the second yesterday, and so on.

## 1. Check how many birds visited today

Implement the `today` function. It should take a list of daily bird counts and return today's count. If the list is empty, it should return `0`.

```gleam
today([2, 5, 1])
// -> 2
```

## 2. Increment today's count

Implement the `increment_day_count` function. It should take a list of daily bird counts and increment the today's count by 1. If the list is empty, return `[1]`.

```gleam
increment_day_count([4, 0, 2])
// -> [5, 0, 2]
```

## 3. Check if there was a day with no visiting birds

Implement the `has_day_without_birds` function. It should take a list of daily bird counts. It should return `True` if there was at least one day when no birds visited the garden, and `False` otherwise. If the list is empty - this counts as no birds being recorded _yet_; return `False`.

```gleam
has_day_without_birds([2, 0, 4])
// -> True

has_day_without_birds([3, 8, 1, 5])
// -> False
```

## 4. Calculate the total number of visiting birds

Implement the `total` function. It should take a list of daily bird counts and return the total number that visited your garden since you started collecting the data.

```gleam
total([4, 0, 9, 0, 5])
// -> 18
```

## 5. Calculate the number of busy days

Some days are busier than others. A busy day is one where five or more birds have visited your garden.

Implement the `busy_days` function. It should take a list of daily bird counts and return the number of busy days.

```gleam
busy_days([4, 5, 0, 0, 6])
// -> 2
```

## Source

### Created by

- @lpil