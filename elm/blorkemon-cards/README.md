# Blorkemon Cards

Welcome to Blorkemon Cards on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Comparison

### Equality

Elm types can be checked for "being the same" with the equality operator `(==)`.
The inequality operator is `(/=)` (*not* `!=` like in many languages).
The equality operator works on literals, tuples, records and custom types.

```elm
1 == 2
    --> False

1 /= 2
    --> True

(1, 2) == (2, 1)
    --> False

type alias MyRecord = { myInt : Int, myStrings : List String }
a : MyRecord
a = MyRecord (1 + 1) ["hello world"]

type alias MyOtherRecord = { myInt : Int, myStrings : List String }
myStrings =  (String.join " " ["hello", "world"]) :: []
b : MyOtherRecord
b = { myInt = 2, myStrings = myStrings}

a == b
    --> True
```

Note that you *should never* compare functions, as this will make the program crash at runtime.

### Comparisons

The comparison operators `(<)`, `(<=)`, `(>)`, `(>=)` and functions `min`, `max`, `compare` work on `comparable` types.
The `comparable` type is a special type that groups all built-in types that can be compared: numbers, characters, strings, lists of comparable things, and tuples of comparable things.

```elm
1 < 1 + 1
    --> True

'x' < 'X'
    --> False

"abc" < "abz"
    --> True

"abc" < "abcd"
    --> True

"abc" < "b"
    --> True

min [1 , 2, 9000] [10]
    --> [1 , 2, 9000]

min (1, "hello") (3, "bye")
    --> (1, "hello")
```

Values of other types such as records or custom types cannot be compared directly.

Some built-in types require their content to be `comparable`, such as `Set` or `Dict` keys, since their structure relies on an internal ordering.

Lists of `comparable` values can be sorted with `List.sort` and lists of values that can be mapped to comparable values can be sorted with `List.sortBy`.
If you need a hierarchical sort (sort by one property, and break ties with another), tuples or lists may also be used with `List.sortBy` since tuples and lists are sorted in lexicographic order.


```elm
List.sort ["hi", "hello", "bye", "goodbye"]
    --> ["bye", "goodbye", "hello", "hi"]

List.sortBy String.length ["hi", "hello", "bye", "goodbye"]
    --> ["hi", "bye", "hello", "goodbye"]

-- sort by length, then alphabetically
List.sortBy (\str -> (String.length str, str)) ["hi", "mum", "hello", "sis", "bye", "dad"]
    --> ["hi", "bye", "dad", "mum", "sis", "hello"]
```

The function `compare` takes two `comparable` values and returns an `Order`.
An `Order` is a type that checks in a in a single operation if a value is greater (`GT`), equal (`EQ`) or less (`LT`) than another.

```elm
compare 1 2
    --> LT

compare "" ""
    --> EQ

compare [12] []
    --> GT
```

Comparison functions can be constructed for arbitrary types and be used, for example, to be sorted with `List.sortWith`:

```elm
type Color = Red | Green | Blue

compareColors : Color -> Color -> Order
compareColors a b =
  case (a, b) of
    (Red, Red) -> EQ
    (Red, _) -> LT
    (_, Red) -> GT
    (Green, Green) -> EQ
    (Green, _) -> LT
    (_, Green) -> GT
    (Blue, Blue) -> EQ

List.sortWith compareColors [Blue, Red, Green, Green, Blue]
    --> [Red, Green, Green, Blue, Blue]
```

## Instructions

You suddenly feel a burst of nostalgia and a strong urge to dust off your old Blorkemon™️ cards.
Blorkemon™️ cards is a great, old school card trading game.
Each `Card` has a Blorkemon™️ monster drawn on it, along with an attack power level, and if you are lucky, the card might even be shiny.

## 1. Compare power levels

Each Blorkemon™️ card looks more powerful than the next, but you better check.

Implement the `isMorePowerful` function. It should return `True` if the card in its first argument is strictly more powerful than the other, and `False` otherwise.

```elm
newthree = Card "Newthree" 120 False
scientuna = Card "Scientuna" 6 True

isMorePowerful newthree scientuna
    --> True
```

## 2. Find the highest power

With a hand full of Blorkemon™️ cards, you should be able to prepare the most devastating attack.

Implement the `maxPower` function, which returns the highest power level of two cards.

```elm
maxPower newthree scientuna
    --> 120
```

## 3. Sort the cards

You seem to remember that you had at least one card of each Blorkemon™️, but it's been a while since you last checked, you should sort your cards to compare them to the official listing on Pulpapedia.

Implement the `sortByMonsterName` function, which should take a list of `Cards` and return a list sorted by monster names.

```elm
sortByMonsterName [newthree, scientuna]
    --> [Card "Newthree" 120 False, Card "Scientuna" 6 True]
```

## 4. Coolest cards first

Blorkemon™️ are the coolest thing ever.
You are not using that term lightly, you have a scientific method to demonstrate it.

Implement the `sortByCoolness` function, which sorts a list of cards by placing the coolest ones first.
The coolness of a card is first determined by its shininess: all shiny cards are way cooler than the others.
The second factor is the power level, the higher the better.

```elm
sortByCoolness [newthree, scientuna]
    --> [Card "Scientuna" 6 True, Card "Newthree" 120 False]
```

## 5. Shiny Power

Shininess is not just for show, in a battle of evenly powered Blorkemon™️, a shiny one will always prevail.
This is called the Shiny Power.

Implement the `compareShinyPower` function, which codifies this property.
The `Order` of two cards is determined by the power levels is they are different, but if they are equal, a shiny card will be greater.

```elm
compareShinyPower newthree scientuna
    --> GT
```

## 6. Place your bets

In a game of Blorkemon™️ cards, anything goes, but there is still a tendency for more powerful cards to win.

Implement the `expectedWinner` function that returns the name of the monster most expected to win, as determined by the `compareShinyPower` ordering.
The function should return the monster name of the expected winner, or "too close to call" if both opponents have the same Shiny Power.

```elm
expectedWinner newthree scientuna
    --> "Newthree"
```

## Source

### Created by

- @jiegillet

### Contributed to by

- @mpizenberg
- @ceddlyburge