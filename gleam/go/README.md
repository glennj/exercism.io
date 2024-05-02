# Go

Welcome to Go on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Results

Gleam doesn't use exceptions for error handling, instead the generic `Result` type is returned by functions that can either succeed or fail.

The `Result` type is built into the language so you don't need to define or import it, but if you were to define it yourself it would look like this:

```gleam
pub type Result(value, error) {
  Ok(value)
  Error(error)
}
```

The `Ok` variant is returned when a function succeeds, and the `Error` variant is returned when a function fails.

Results are very common in Gleam, and Gleam programmers will commonly use the [`gleam/result` module](https://hexdocs.pm/gleam_stdlib/gleam/result.html) to make working with them easier.

The `result.map` function can be used to call a function on the value inside a result if it is an `Ok`, or to pass through an `Error` unchanged.

```gleam
Ok(1)
|> result.map(fn(x) { x + 1 })
// -> Ok(2)

Error("Oh no!")
|> result.map(fn(x) { x + 1 })
// -> Error("Oh no!")
```

The `result.try` function is similar, but the callback function is expected to return a result. This is useful for chaining together multiple functions that return results.

```gleam
Ok(1)
|> result.try(fn(x) { Ok(x + 1) })
// -> Ok(2)

Ok(1)
|> result.try(fn(x) { Error("Nope!") })
// -> Error("Nope!")
```

## Instructions

In this exercise, you'll be applying the [rules of the game of Go](https://matmoore.github.io/learngo/). The rules themselves are already written, you just need to apply them in order to update the Game and to handle any errors / violations of the rules.

The game is represented as follows:

```gleam
pub type Player {
  Black
  White
}


pub type Game {
  Game(
    white_captured_stones: Int,
    black_captured_stones: Int,
    player: Player,
    error: String,
  )
}
```

There are 4 rules in the game:

- Each point can only have one stone.
- Opposition stones can be captured.
- You can not place a stone where it would capture itself.
- You can not use the same point twice.

## 1. Apply the rules

Write the content of the `apply_rules` function, which takes an initial `Game` and a set of rules, and returns the new `Game` after the rules have been applied.

Three of the rules all check for violations of the rules and may return an error, and so have the return type of `Result(Game, String)`. If any of these rules fail, the original game should be returned, but with the `error` field updated with the relevant error.

The other rule does not check for violations and so cannot fail (although it can return a changed `Game`), and so has the return type of `Game`.

If all the rules pass, then any changes to `Game` from the rules should be kept, and the player should be changed.

```gleam
pub fn apply_rules(
  game: Game,
  rule1: fn(Game) -> Result(Game, String),
  rule2: fn(Game) -> Game,
  rule3: fn(Game) -> Result(Game, String),
  rule4: fn(Game) -> Result(Game, String),
) -> Game {
  // -> If all rules pass, return a `Game` with all changes from the rules applied, and change player
  // -> If any rule fails, return the original Game, but with the error field set
}
```

## Source

### Created by

- @lpil