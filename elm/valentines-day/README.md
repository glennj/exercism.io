# Valentines Day

Welcome to Valentines Day on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Custom Types

[Custom Types][custom-types] in Elm represent a fixed number of named cases. Each value corresponds to exactly one of the named cases.

A Custom Type is defined using the `type` keyword and requires very little syntax. They are one of the most important techniques in Elm programming, and are used to make the possible values in code exactly match the valid values in real life, which leaves no room for invalid data, and makes [impossible states impossible to represent in the code][impossible-states].

```elm
type Season
    = Spring
    | Summer
    | Autumn
    | Winter
```

Each case of a custom type can optionally have data associated with it, and different cases can have different types of data. If none of the cases have data associated with them, the discriminated union is similar to what other languages usually refer to as an _enumeration_ (or _enum_).

```elm
type FlexibleNumber
    = Integer Int
    | Float Float
    | Invalid
```

Creating a value for a specific case can be done by referring to its name (`Spring`), when it is defined in the same module, or is imported with the `import SeasonModule exposing (Season(..))` style syntax, or by referring to its fully qualified name (`Season.Spring`) when imported with `import SeasonModule exposing (Season)` style syntax.

```elm
integerTwo = Integer 2
invalid = FlexibleNumber.Invalid
```

Custom types, along with everything in Elm, have _structural equality_, which means that two values for the same case and with the same (optional) data are equivalent.

The preferred way to work with custom types is with [pattern matching][pattern-matching]:

```elm
let describe flexibleNumber =
    case flexibleNumber of
        Integer i ->
            "Integer: " ++ fromInt(i)

        Float f ->
            "Float: " ++ fromFloat(f)

        Invalid   ->
            "Invalid"
```

[custom-types]: https://guide.elm-lang.org/types/custom_types.html
[pattern-matching]: https://guide.elm-lang.org/types/pattern_matching.html
[impossible-states]: https://www.youtube.com/watch?v=IcgmSRJHu_8

## Instructions

In this exercise, it's Valentine's day and you and your partner are planning on doing something nice together. Your partner has lots of ideas, and is now asking you to rate the ideas, in order to find the activity to engage in.

The following ideas are proposed by your partner:

- Playing a board game
- Chill out
- Watch a movie
- Go to a restaurant

You have six tasks to help choose your Valentine's day activity.

## 1. Define the approval

For each idea your partner proposes, you respond with one of three options: yes, no or maybe.

Define the `Approval` Custom Type to represent these options as the following three cases: `Yes`, `No` and `Maybe`.

## 2. Define the cuisines

Your partner has selected two possible restaurants: one based on the Korean cuisine and the other based on the Turkish cuisine.

Define the `Cuisine` Custom Type to represent these cuisines as the following two cases: `Korean` and `Turkish`.

## 3. Define the movie genres

There are tons of movies to choose from, so to narrow things down, your partner also lists their genre.

Define the `Genre` Custom Type to represent the following genres as cases: `Crime`, `Horror`, `Romance` and `Thriller`.

## 4. Define the activity

As said, your partner has come up with five different activities: playing a board game, chill out, watch a movie and go to a restaurant.

Define the `Activity` Custom Type to represent these activity types:

- `BoardGame`: no associated data.
- `Chill`: no associated data.
- `Movie`: has its `Genre` as associated data.
- `Restaurant`: has its `Cuisine` as associated data.

## 5. Rate the activity

Finally, you're ready to rate your partner's ideas. This is how you feel about your partner's idea:

- Playing a board game: no.
- Chill out: no.
- Watch a movie: yes if is is a romantic movie; otherwise, no.
- Go to a restaurant: yes if the cuisine is Korean, maybe if it is Turkish.

Implement a function named `rateActivity` that takes an `Activity` value and returns the `Approval` based on the above sentiments:

```elm
rateActivity (Restaurant Turkish)
-- => Maybe
```

## Source

### Created by

- @ceddlyburge

### Contributed to by

- @mpizenberg