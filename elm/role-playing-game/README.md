# Role Playing Game

Welcome to Role Playing Game on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Maybe

The `Maybe` type is the solution in the Elm language for optional values.
It is thus present in type signatures of a wide number of core Elm functions and understanding it is crucial.
The `Maybe` type is defined as follows:

```elm
type Maybe a = Nothing | Just a
```

This is known as a "custom type" definition in Elm terminology.
It indicates that a value of this type can either be `Nothing` OR be `Just` something of type `a`.
Creating a `Maybe` value is done via one of its two constructors `Nothing` and `Just`.
Reading the content of a `Maybe` is done via pattern matching.

```elm
matthieu : Maybe String
matthieu = Just "Matthieu"

anon : Maybe String
anon = Nothing

sayHello : Maybe String -> String
sayHello maybeName =
    case maybeName of
        Nothing -> "Hello, World!"
        Just someName -> "Hello, " ++ someName ++ "!"

sayHello matthieu
    --> "Hello, Matthieu!"

sayHello anon
    --> "Hello, World!"
```

There are also a number of useful functions in the `Maybe` module to manipulate `Maybe` types.

```elm
sayHelloAgain : Maybe String -> String
sayHelloAgain name = "Hello, " ++ Maybe.withDefault "World" name ++ "!"

capitalizeName : Maybe String -> Maybe String
capitalizeName name = Maybe.map String.toUpper name

capitalizeName matthieu
    --> Just "MATTHIEU"

capitalizeName anon
    --> Nothing
```

## Instructions

In this exercise, you'll be implementing mechanics of a role-playing game.
A player's character is represented by the following type:

```elm
type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }
```

Players in this game must reach level 10 before they unlock a mana pool so that they can start casting spells.
You're working on two pieces of functionality in this game, the revive mechanic and the spell casting mechanic.

## 1. Introduce yourself

Write the content of the `introduce` function.
Stealthy players may be hiding their name and will be introduced as `"Mighty Magician"`.
Otherwise, just use your name to introduce yourself.

```elm
introduce { name = Nothing, level = 2, health = 8, mana = Nothing }
    --> "Mighty Magician"

introduce { name = Just "Merlin", level = 2, health = 8, mana = Nothing }
    --> "Merlin"
```

## 2. Implement the revive mechanic

The `revive` function should check that the player's character is indeed dead (their health has reached 0).
If they are, it should return a new `Player` instance with 100 health.
Otherwise, if the player's character isn't dead, the `revive` function returns `Nothing`.

If the player's level is 10 or above, they should also be revived with 100 mana.
If they player's level is below 10, their mana should be `Nothing`.
The `revive` function should preserve the player's level.

```elm
deadPlayer = { name = Nothing, level = 2, health = 0, mana = Nothing }

revive deadPlayer
    --> Just { name = Nothing, level = 2, health = 100, mana = Nothing }
```

If the `revive` method is called on a player whose health is 1 or above, then the function should return `Nothing`.

```elm
alivePlayer = { name = Nothing, level = 2, health = 42, mana = Nothing }

revive alivePlayer
    --> Nothing
```

## 3. Implement the spell casting mechanic

The `castSpell` function takes as arguments an `Int` indicating how much mana the spell costs as well as a `Player`.
It returns the updated player, as well as the amount of damage that the cast spell performs.
A successful spell cast does damage equal to two times the mana cost of the spell.
However, if the player has insufficient mana, nothing happens, the player is unchanged and no damage is done.
If the player does not even have a mana pool, attempting to cast the spell must decrease their health by the mana cost of the spell and does no damage.

```elm
wizard = { name = Nothing, level = 18, health = 123, mana = Just 30 }

( updatedWizard, damage ) = castSpell 14 wizard

updatedWizard.mana --> Just 16
damage             --> 28
```

## Source

### Created by

- @mpizenberg

### Contributed to by

- @ceddlyburge