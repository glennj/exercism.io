# Monster Attack

Welcome to Monster Attack on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Partial application and function composition

### Partial application

All functions in Elm are [curried][currying], which means that if you call a function without passing all of the arguments, it returns a new function.

The result of doing this is called [partial-application][partial-application] (you are not passing all the arguments to the function, but are partially applying some of them).

```elm
add: Int -> Int -> Int
add a b = a + b

add2: Int -> Int
add2 = add 2

eight: Int
eight = add2 6
--> 8
```

Partial application always applies the arguments in order.

```elm
contains: String -> String -> Bool
contains first second =
--> This function is in the standard library, and returns True if the second string contains the first one

containsCedd: String -> Bool
containsCedd = contains "Cedd"
--> (partial application always applies the arguments in order, so "Cedd" becomes the first argument to contains
--> This is a function that takes a string, and returns True if that string contains Cedd)

ceddBurgeContainsCedd: Bool
ceddBurgeContainsCedd = containsCedd "Cedd Burge"
--> True
--> ("Cedd Burge" ends up being the second argument to contains, and it does include "Cedd", the first argument)
```

### Pipe operator (`|>`)

Saying `x |> f` is exactly the same as `f x`. A concrete example is `"5" |> String.toInt` is the same as `String.toInt "5"`.

This doesn't sound very useful at first, but it is called the [pipe operator][pipe-operator] because it lets you write “pipelined” code.

For example, say we have a sanitize function for turning user input into integers:

```elm
sanitize : String -> Maybe Int
sanitize input =
    String.toInt (String.trim input)
```

We can rewrite it like this:

```elm
sanitize : String -> Maybe Int
sanitize input =
    input
    |> String.trim
    |> String.toInt
```

This avoids the use of brackets, and shows the functions in the order that they are applied (first `String.trim`, then `String.toInt`), which aids readability.

There is also a [reverse pipe operator `(<|)`][reverse-pipe-operator].
Saying `f <| x` is exactly the same as `f x`.
This doesn't look very useful at first glance either, and is definitely not used as much as the pipe operator, but it helps to avoid brackets in some situations.

### Function composition / point free style

The [`(>>)`][forward-composition] operator has the type `(a -> b) -> (b -> c) -> (a -> c)` and creates a function of type `a -> c` by composing two compatible functions of type `a -> b `and `b -> c`.
It does this by taking the output (of type `b`) from the first function and using it as the first argument to the second function (which must be of the same type `b`).
The created function will take the same input (of type `a`) first function, and will return whatever `c` the second function returns.

A concrete example is `String.trim >> String.toInt`.
`String.trim` takes a string as its only parameter, so this is what the created function takes.
`String.toInt` returns a `Maybe Int`, so this is what the created function returns.
`String.trim` returns a `String`, which `String.toInt` accepts as a parameter, so the example compiles.

We can now rewrite our `sanitize` function as follows:

```elm
sanitize : String -> Maybe Int
sanitize =
    String.trim >> String.toInt
```

Note that we no longer use the `input` parameter that you can see the original example, even though the functionality is identical.
This way of creating functions that make no reference to their parameters is called "Point Free Style".
The advantage of this style is that the code is more concise, the disadvantage is that it can be harder to understand, especially for beginners.

It is worth remembering that all functions are curried in Elm, so `String.length >> max` compiles.

- The [`(>>)`][forward-composition] operator has the type `(a -> b) -> (b -> c) -> (a -> c)`.
- `String.length` has a type of `String -> Int`, which is the `(a -> b)` part, so `a` is `String` and `b` is - `Int`.
- `max` has a type of `Int -> Int -> Int`, which is the `b -> c` part.
- `b` is `Int`, so `max` is partially applied, so `c` is the result of this partial application (`Int -> Int`).
- So the type of `String.length >> max` is `String -> (Int -> Int)`.
- So `((String.length >> max) "123") 2` returns `3`, because `"123"` has a length of 3, which is greater than the 2 from the `Int` argument.

### Function design

To make it easy to write elegant code in Elm, you should make the main data structure the last parameter of functions.
All the common functions in the standard library do this, such as `List.map` (type signature below).

```elm
map : (a -> b) -> List a -> List b
```

Here we can see that the last parameter to the `map` function is `List a` and it returns `List b`, so the main data structure (`List`) is the last parameter.

This makes it possible to write elegant pipelined code, such as below.

```elm
trimAndUpperCase : List String -> List String
trimAndUpperCase names =
    names
    |> List.map String.trim
    |> List.map String.toUpper
```

It also makes it convenient to use function composition and point free style, as an alternative to pipelining.

```elm
trimAndUpperCase : List String -> List String
trimAndUpperCase =
    List.map String.trim
    >> List.map String.toUpper
```

Function composition also allows you to reduce this example to a single call to `List.map`.

```elm
trimAndUpperCase : List String -> List String
trimAndUpperCase =
    List.map (String.trim >> String.toUpper)
```

[currying]: https://www.bekk.christmas/post/2020/9/hurry-curry!
[partial-application]: https://www.bekk.christmas/post/2020/10/partial-application-of-functions!
[pipe-operator]: https://package.elm-lang.org/packages/elm/core/latest/Basics#|%3E
[reverse-pipe-operator]: https://package.elm-lang.org/packages/elm/core/latest/Basics#%3C|
[forward-composition]: https://package.elm-lang.org/packages/elm/core/latest/Basics#%3E%3E
[backward-composition]: https://package.elm-lang.org/packages/elm/core/latest/Basics#%3C%3C

## Instructions

In this exercise, you'll be implementing the quest logic for a new RPG game a friend is developing.
The game's main character is Annalyn, a brave girl with a fierce and loyal pet dog Kazak.
At one point they are ambushed by a monster which they attack.

## 1. Define attackWithSword1

Define `attackWithSword1`.
This should take a `MonsterDamage` (a `String` that describes the damage to the monster) and an `Int` that indicates how strong the attack is.
It should return the existing string, with "Attacked with sword of strength {strength}." appended to the end.

## 2. Define attackWithClaw1

Define `attackWithClaw1`.
This should take a `MonsterDamage` and an `Int` that indicates how strong the attack is.
It should return the existing string, with "Attacked with claw of strength {strength}." appended to the end.

## 3. Define attack1

Annalyn attacks with a sword, and she has a strength of 5.

Her loyal pet dog Kazak attacks with its claws, and has a strength of 1.

Define `attack1`. This should take a `MonsterDamage` and perform the following attacks, in the correct order:

- Annalyn
- Kazak
- Kazak
- Annalyn

The implementation of this function will be a bit cumbersome, as `attackWithSword1` and `attackWithClaw1` do not have the main data structure (`MonsterDamage`) as the last parameter (they are not doing it in the recommended way).
It is not possible / advisable to use partial application in this initial case.

## 4. Define attackWithSword2

Define `attackWithSword2`.
This is the same as `attackWithSword1`, except the parameters are swapped.

## 5. Define attackWithClaw2

Define `attackWithClaw2`.
This is the same as `attackWithClaw2`, except the parameters are swapped.

## 6. Define attack2

Define `attack2`.

This is the same as `attack1`, but should use `attackWithSword2` and `attackWithClaw2`.

Please implement the function using [Partial application][partial-application] and the [pipe operator `(|>)`][pipe-operator].
The implementation should now be more elegant, as `attackWithSword2` and `attackWithClaw2` have `MonsterDamage` (the main data structure) as the last parameter, as is recommended.

## 7. Define attack3

Define `attack3`.

This is the same as `attack2`, but should be implemented using function composition / the [`(>>)`][forward-composition] operator.

This implementation should also be elegant, and smaller than `attack2`, although potentially harder to understand.

[pipe-operator]: https://package.elm-lang.org/packages/elm/core/latest/Basics#|%3E
[partial-application]: https://riptutorial.com/elm/example/7499/partial-application
[forward-composition]: https://package.elm-lang.org/packages/elm/core/latest/Basics#%3E%3E

## Source

### Created by

- @ceddlyburge