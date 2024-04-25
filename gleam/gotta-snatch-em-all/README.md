# Gotta Snatch 'Em All

Welcome to Gotta Snatch 'Em All on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Sets

A set is an immutable and unordered collection of unique values. All items in a set have to be of the same type.

The `gleam/set` module defines the `Set` type and provides functions for working with sets.

You can transform lists and sets into each other with `set.to_list` and `set.from_list`. The order of items in a set is not guaranteed.

```gleam
set.from_list([2, 3, 3, 3, 1, 1, 2])
// -> set.from_list([1, 2, 3])

set.to_list(set.from_list([2, 3, 3, 3, 1, 1, 2]))
// -> [1, 2, 3]
```

You can create and populate sets with `set.new`, `set.insert` and `set.delete`.

```gleam
set.new()
// -> set.from_list([])

let eighty_eight = set.from_list([88])

set.insert(eighty_eight, 88)
// -> set.from_list([88])

set.insert(eighty_eight, 89)
// -> set.from_list([88, 89])

set.delete(eighty_eight, 88)
// -> set.from_list([])

set.delete(eighty_eight, 89)
// -> set.from_list([88])
```

You can query the contents of a set with the functions `set.contains`, and `set.size`.

```gleam
set.contains(eighty_eight, 88)
// -> True

set.size(eighty_eight)
// -> 1
```

Set can be combined with `set.union`, and `set.intersect`.

```gleam
let a = set.from_list([1, 5, 10])
let b = set.from_list([1, 2, 3, 4, 5])

set.union(a, b)
// -> set.from_list([1, 2, 3, 4, 5, 10])

set.intersection(a, b)
// -> set.from_list([1, 5])
```

You can filter sets.

```gleam
let is_small = fn(n) { x <= 3 }

set.filter(b, is_small)
// -> set.from_list([1, 2, 3])
```

## Instructions

Your nostalgia for Blorkemon™️ cards is showing no sign of slowing down, you even started collecting them again, and you are getting your friends to join you. 

In this exercise, a card collection is represented by `Set(String)`, since duplicate cards are not important when your goal is to get all existing cards.

## 1. Start a collection

You really want your friends to join your Blorkemon™️ madness, and the best way is to kickstart their collection by giving them one card.

Implement `new_collection`, which transforms a card into a collection.

```gleam
new_collection("Newthree")
// -> set.from_list(["Newthree"])
```

## 2. Grow the collection

Once you have a collection, it takes a life of its own and must grow.

Implement `add_card`, which takes a card and a collection, and returns a tuple with two values: a `Bool` that indicates if the card was already in the collection, and the collection with the card added.

```gleam
add_card("Scientuna" set.from_list(["Newthree"]))
// -> #(False, set.from_list(["Newthree", "Scientuna"]))
```

## 3. Start trading

Now that your friends are Blorkemon™️ crazy again, you can use this to grow your own collection by trading cards.

Not every trade is worth doing, or can be done at all.
You cannot trade a card you don't have, and you shouldn't trade a card for one that you already have. 

Implement `trade_card`, that takes two cards to trade (yours and theirs) and your current collection.
The return value is a tuple of two values: a `Bool` stating if the trade is possible and worth doing, and the collection you would end up with if you did the trade (even if it's not actually possible).

```gleam
trade_card("Scientuna", "Newthree", set.from_list(["Scientuna"]))
// -> #(True, set.from_list(["Newthree"]))
```

## 4. Cards they all have

You and your Blorkemon™️ enthusiast friends gather and wonder which cards are the most common.

Implement `boring_cards`, which takes a list of collections and returns a list of sorted cards that all collections have.

```gleam
boring_cards([set.from_list(["Scientuna"]), set.from_list(["Newthree", "Scientuna"])])
// -> ["Scientuna"]
```

## 5. All of the cards

Do you and your friends collectively own all of the Blorkemon™️ cards?

Implement `total_cards`, which takes a list of collections and returns the total number of different cards in all of the collections.

```gleam
total_cards([set.from_list(["Scientuna"]), set.from_list(["Newthree", "Scientuna"])])
// -> 2
```

## 6. Shiny for the win

Your nephew is coming to visit you soon, and you feel like impressing him.
Kids like shiny things right?
Blorkemon™️ cards can be shiny!

Implement `shiny_cards`, which takes a collection and returns a set containing all the cards that start with `"Shiny "`.

```gleam
shiny_cards(set.from_list(["Newthree", "Scientuna", "Shiny Scientuna"]))
// -> set.from_list(["Shiny Scientuna"])
```

## Source

### Created by

- @lpil