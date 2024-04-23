# Valentine's Day

Welcome to Valentine's Day on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Custom Types

Custom Types are how new data types are defined in Gleam. They can have multiple _variants_, each with their own name.

```gleam
pub type Season {
  Spring
  Summer
  Autumn
  Winter
}
```

Each case of a custom type can optionally hold some data, and different cases can have different types of data. When a variant holds data it is called a _record_.

```gleam
pub type Number {
  SomeInt(Int)
  SomeFloat(Float)
  Invalid
}
```

Creating a value for a specific case can be done by referring to its name if it contains no additional data (`Spring`), or by calling it as a function if it does (`SomeInt(2)`).

```gleam
let spring = Spring
let integerTwo = SomeInt(2)
```

Custom types, along with everything in Gleam, have _structural equality_, which means that two values of the same variant and with the same data are equivalent.

```gleam
Spring == Spring // -> True
Spring == Autumn // -> False
SomeInt(2) == SomeInt(2) // -> True
SomeInt(2) == SomeFloat(2.0) // -> False
```

Custom type variants can be pattern matched on using case expressions.

```gleam
import gleam/int
import gleam/float

pub fn describe(flexible_number: Number) -> String {
  case flexible_number {
    SomeFloat(f) -> "Float: " <> float.to_string(f)
    SomeInt(i) -> "Int: " <> int.to_string(i)
    Invalid -> "Neither a float nor an int"
  }
}
```

## Instructions

In this exercise, it's Valentine's day and you and your partner are planning on doing something nice together. Your partner has lots of ideas, and is now asking you to rate the ideas, in order to find the activity to engage in.

The following ideas are proposed by your partner:

- Playing a board game
- Chill out
- Watch a movie
- Go to a restaurant
- Take a walk

You have six tasks to help choose your Valentine's day activity.

## 1. Define the approval

For each idea your partner proposes, you respond with one of three options: yes, no or maybe.

Define the `Approval` custom type to represent these options as the following three cases: `Yes`, `No` and `Maybe`.

## 2. Define the cuisines

Your partner has selected two possible restaurants: one based on the Korean cuisine and the other based on the Turkish cuisine.

Define the `Cuisine` custom type to represent these cuisines as the following two cases: `Korean` and `Turkish`.

## 3. Define the movie genres

There are tons of movies to choose from, so to narrow things down, your partner also lists their genre.

Define the `Genre` custom type to represent the following genres as cases: `Crime`, `Horror`, `Romance` and `Thriller`.

## 4. Define the activity

As said, your partner has come up with five different activities: playing a board game, chill out, watch a movie, go to a restaurant and taking a walk.

Define the `Activity` custom type to represent these activity types:

- `BoardGame`: no associated data.
- `Chill`: no associated data.
- `Movie`: has its `Genre` as associated data.
- `Restaurant`: has its `Cuisine` as associated data.
- `Walk`: has an `Int` representing the number of kilometers to walk as associated data.

## 5. Rate the activity

Finally, you're ready to rate your partner's ideas. This is how you feel about your partner's idea:

- Playing a board game: no.
- Chill out: no.
- Watch a movie: yes if it is a romantic movie; otherwise, no.
- Go to a restaurant: yes if the cuisine is Korean, maybe if it is Turkish.
- Take a walk: yes if the walk is more than eleven kilometers; maybe if it is more than six kilometers; otherwise, no.

Implement a function named `rate_activity` that takes an `Activity` value and returns the `Approval` based on the above sentiments:

```gleam
rate_activity(Restaurant(Turkish))
// -> Maybe
```

## Source

### Created by

- @lpil