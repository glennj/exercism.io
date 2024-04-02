# Gotta Snatch'Em All

Welcome to Gotta Snatch'Em All on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Set

A `Set a` is a sequence of unique values of type `a`.
The values have to be of the `comparable` type (`Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists of comparable types).

You can transform lists and sets into each other with `Set.toList` and `Set.fromList`.
Because of the internal structure of `Set`, it is guaranteed that a list obtained from `Set.fromList` will be sorted.

```elm
Set.fromList [2, 3, 3, 3, 1, 1, 2]
    --> Set.fromList [1, 2, 3]

Set.toList (Set.fromList [2, 3, 3, 3, 1, 1, 2])
    --> [1, 2, 3]
```

You can create and populate sets with `Set.empty`, `Set.singleton`, `Set.insert` and `Set.remove`.

```elm
Set.empty
    --> Set.fromList []

Set.singleton "hi"
    --> Set.fromList ["hi"]

eightyEight = Set.singleton 88

Set.insert 88 eightyEight
    --> Set.fromList [88]

Set.insert 89 eightyEight
    --> Set.fromList [88, 89]

Set.remove 88 eightyEight
    --> Set.fromList []

Set.remove 89 eightyEight
    --> Set.fromList [88]
```

You can query the contents of a set with the functions `Set.member`, `Set.isEmpty` and `Set.size`.

```elm
Set.member 88 eightyEight
    --> True

Set.isEmpty eightyEight
    --> False

Set.size eightyEight
    --> 1
```

Set can be combined with `Set.union`, `Set.intersect` and `Set.diff`.

```elm
a = Set.fromList [1, 10, 100]
b = Set.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

Set.union a b
    --> Set.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100]

Set.intersect a b
    --> Set.fromList [1, 10]

Set.diff a b
    --> Set.fromList [100]

Set.diff b a
    --> Set.fromList [2, 3, 4, 5, 6, 7, 8, 9]
```

You can filter and partition sets.

```elm
even n = modBy 2 n == 0

Set.filter even b
    --> Set.fromList [2, 4, 6, 8, 10]

Set.partition even b
    --> (Set.fromList [2, 4, 6, 8, 10], Set.fromList [1, 3, 5, 7, 9])
```

## Instructions

Your nostalgia for Blorkemon™️ cards is showing no sign of slowing down, you even started collecting them again, and you are getting your friends to join you. 

In this exercise, a `Card` collection is represented by `Set Card`, since duplicate cards are not important when your goal is to get all existing cards.

## 1. Start a collection

You really want your friends to join your Blorkemon™️ madness, and the best way is to kickstart their collection by giving them one card.

Implement `newCollection`, which transforms a card into a collection.

```elm
newCollection "Newthree"
    --> Set.fromList ["Newthree"]
```

## 2. Grow the collection

Once you have a collection, it takes a life of its owm and must grow.

Implement `addCard`, which takes a `Card` and a collection, and returns a `Tuple` with two values: a `Bool` that indicates if the `Card` was already in the collection, and the collection with the card added.

```elm
addCard "Scientuna" (Set.fromList ["Newthree"])
    --> (False, Set.fromList ["Newthree", "Scientuna"])
```

## 3. Start trading

Now that your friends are Blorkemon™️ crazy again, you can use this to grow your own collection by trading cards.

Not every trade is worth doing, or can be done at all.
You cannot trade a card you don't have, and you shouldn't trade a card for one that you already have. 

Implement `tradeCard`, that takes two cards to trade (yours and theirs) and your current collection.
The return value is a `Tuple` of two values: a `Bool` stating if the trade is possible and worth doing, and the collection you would end up with if you did the trade (even if it's not actually possible).

```elm
tradeCard "Scientuna" "Newthree" (Set.fromList ["Scientuna"])
    --> (True, Set.fromList ["Newthree"])
```

## 4. There can be only one of each

You find an old stash of cards in a flea market.
You must sort the cards and remove the duplicates to compare the list to your collection.

Implement `removeDuplicates` which will sort a list of cards and return a list of sorted, unique cards.

```elm
removeDuplicates ["Newthree", "Newthree", "Newthree", "Scientuna"]
    --> ["Newthree", "Scientuna"]
```

## 5. Cards they don't have

Time to feel good about your collection.

Implement `extraCards`, which takes your collection and some other collection, and returns the number of cards that the other collection doesn't have.

```elm
extraCards (Set.fromList ["Scientuna"]) (Set.fromList ["Newthree", "Scientuna"])
    --> 0
```

## 6. Cards they all have

You and your Blorkemon™️ enthusiast friends gather and wonder which cards are the most common.

Implement `boringCards`, which takes a list of collections and returns a list of sorted cards that all collections have.

```elm
boringCards [Set.fromList ["Scientuna"], Set.fromList ["Newthree", "Scientuna"]]
    --> ["Scientuna"]
```

## 7. All of the cards

Do you and your friends collectively own all of the Blorkemon™️ cards?

Implement `totalCards`, which takes a list of collections and returns the total number of different cards in the all of the collections.

```elm
totalCards [Set.fromList ["Scientuna"], Set.fromList ["Newthree", "Scientuna"]]
    --> 2
```

## 8. Shiny for the win

You nephew is coming to visit you soon, and you feel like impressing him.
Kids like shiny things right?
Blorkemon™️ cards can be shiny!

Implement `splitShinyCards`, which takes a collection and returns a `Tuple` with two lists of sorted cards: one with all the cards that start with `"Shiny"` and one with the other cards. 

```elm
splitShinyCards (Set.fromList ["Newthree", "Scientuna", "Shiny Scientuna"])
    --> (["Shiny Scientuna"], ["Newthree", "Scientuna"])
```

## Source

### Created by

- @jiegillet

### Contributed to by

- @ceddlyburge