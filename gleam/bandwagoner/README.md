# Bandwagoner

Welcome to Bandwagoner on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Labelled Fields

When a custom type variant holds data it is called a record, and each contained value resides in a _field_.

```gleam
pub type Rectangle {
  Rectangle(
    Float, // The first field
    Float, // The second field
  )
}
```

To aid readability Gleam allows fields to be labelled with a name.

```gleam
pub type Rectangle {
  Rectangle(
    width: Float,
    height: Float,
  )
}
```

Labels can be used to give arguments in any order to the constructor of a record.

```gleam
let a = Rectangle(height: 10.0, width: 20.0)
let b = Rectangle(width: 20.0, height: 10.0)

a == b
// -> True
```

When a custom type has only one variant then the `.label` accessor syntax can be used to get the fields of a record.

```gleam
let rect = Rectangle(height: 10.0, width: 20.0)

rect.height // -> 10.0
rect.width  // -> 20.0
```

The record update syntax can be used when a custom type has a single variant to create a new record from an existing one, but with some of the fields replaced with new values.

```gleam
let rect = Rectangle(height: 10.0, width: 20.0)
let tall_rect = Rectangle(..rect, height: 50.0)

tall_rect.height // -> 50.0
tall_rect.width  // -> 20.0
```

Labels can also be used when pattern matching to extract values from records.

```gleam
pub fn is_tall(rect: Rectangle) {
  case rect {
    Rectangle(height: h, width: _) if h > 20.0 -> True
    _ -> False
  }
}
```

If we only want to match on some of the fields we can use the spread operator `..` to ignore any remaining fields.

```gleam
pub fn is_tall(rect: Rectangle) {
  case rect {
    Rectangle(height: h, ..) if h > 20.0 -> True
    _ -> False
  }
}
```

## Instructions

In this exercise you're a big sports fan and you've just discovered a passion for NBA basketball. Being new to NBA basketball, you're doing a deep dive into NBA history, keeping track of teams, coaches, their win/loss stats, and comparing them against each other.

As you don't yet have a favorite team, you'll also be developing an algorithm to figure out whether to root for a particular team.

You have seven tasks to help you develop your proprietary _root-for-a-team_ algorithm.

## 1. Define the model

Define the `Coach` record with the following two fields:

- `name`: the coach's name, of type `String`.
- `former_player`: indicates if the coach was a former player, of type `Bool`.

Define the `Stats` record with the following two fields:

- `wins`: the number of wins, of type `Int`.
- `losses`: the number of losses, of type `Int`.

Define the `Team` record with the following three fields:

- `name`: the team's name, of type `String`.
- `coach`: the team's coach, of type `Coach`.
- `stats`: the team's stats, of type `Stats`.

## 2. Create a team's coach

Implement the `create_coach` function that takes the coach name and its former player status as parameters, and returns its `Coach` record:

```gleam
create_coach("Larry Bird", True)
// -> Coach(name: "Larry Bird", former_player: True)
```

## 3. Create a team's stats

Implement the `create_stats` function that takes the number of wins and the number losses as parameters, and returns its `Stats` record:

```gleam
create_stats(58, 24)
// -> Stats(wins: 58, losses: 24)
```

## 4. Create a team

Implement the `create_team` function that takes the team name, coach and record as parameters, and returns its `Team` record:

```gleam
let coach = create_coach("Larry Bird", True)
let stats = create_stats(58, 24)
create_team("Indiana Pacers", coach, stats)
// -> Team(
//   name: "Indiana Pacers",
//   coach: Coach(name: "Larry Bird", FormerPlayer = True),
//   stats: Stats(wins: 58, losses: 24),
// ) 
```

## 5. Replace the coach

NBA owners being impatient, you found that bad team results would often lead to the coach being replaced. Implement the `replace_coach` function that takes the team and its new coach as parameters, and returns the team but with the new coach:

```gleam
let coach = create_coach("Larry Bird", True)
let record = create_stats(58, 24)
let team = create_team("Indiana Pacers", coach, record)

let new_coach = create_coach("Isiah Thomas", True)
replace_coach(team, new_coach)
// -> Team(
//   name: "Indiana Pacers",
//   coach: Coach(name: "Isiah Thomas", FormerPlayer = True),
//   stats: Stats(wins: 58, losses: 24),
// ) 
```

## 6. Check for same team

While digging into stats, you're keeping lists of teams and their records. Sometimes, you get things wrong and there are duplicate entries on your list. Implement the `is_same_team` function that takes two teams and returns `True` if they are the same team, otherwise, return `False`:

```gleam
let pacers_coach = create_coach("Larry Bird", True)
let pacers_stats = create_stats(58, 24)
let pacers_team = create_team("Indiana Pacers", pacers_coach, pacers_stats)

let lakers_coach = create_coach("Del Harris", False)
let lakers_stats = create_stats(61, 21)
let lakers_team = create_team("LA Lakers", lakers_coach, lakers_stats)

is_same_team(pacers_team, lakers_team)
// -> False
```

## 7. Check if you should root for a team

Having looked at many teams and matches, you've come up with an algorithm. If one of the following is True, you root for that team:

- The coach's name is "Gregg Popovich"
- The coach is a former player
- The team's name is the "Chicago Bulls"
- The team has won 60 or more games
- The team has more losses than wins

Implement the `root_for_team` function that takes a team and returns `True` if you should root for that team, otherwise, return `False`:

```gleam
let spurs_coach = create_coach("Gregg Popovich", False)
let spurs_stats = create_stats(56, 26)
let spurs_team = create_team("San Antonio Spurs", spurs_coach, spurs_stats)
root_for_team(spurs_team)
// -> True
```

## Source

### Created by

- @lpil