# Gotta Snatch 'Em All

Welcome to Gotta Snatch 'Em All on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Sets

A [`Set`][set-docs] is an unordered collection that (unlike `List`) is guaranteed to not contain any duplicate values.

The generic type parameter of the `Set` interface denotes the type of the elements contained in the `Set`:

```java
Set<Integer> ints = Set.of(1, 2, 3);
Set<String> strings = Set.of("alpha", "beta", "gamma");
Set<Object> mixed = Set.of(1, false, "foo");
```

Note that the `Set.of()` method creates an [_unmodifiable_][unmodifiable-set-docs] `Set` instance.
Trying to call methods like `add` and `remove` on this instance will result in an exception at run-time.

To create a modifiable `Set`, you need to instantiate a class that implements the `Set` interface.
The most-used built-in class that implements this interface is the [`HashSet`][hashset-docs] class.

```java
Set<Integer> ints = new HashSet<>();
```

The `Set` interface extends from the [`Collection`][collection-docs] and [`Iterable`][iterable-docs] interfaces, and therefore shares a lot of methods with other types of collections.
A notable difference to the `Collection` interface, however, is that methods like `add` and `remove` return a `boolean` (instead of `void`) which indicates whether the item was contained in the set when that method was called:

```java
Set<Integer> set = new HashSet<>();
set.add(1);
// => true
set.add(2);
// => true
set.add(1);
// => false
set.size();
// => 2
set.contains(1);
// => true
set.contains(3);
// => false
set.remove(3);
// => false
set.remove(2);
// => true
set.size();
// => 1
```

[collection-docs]: https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/Collection.html
[hashset-docs]: https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/HashSet.html
[iterable-docs]: https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/Iterable.html
[set-docs]: https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/Set.html
[unmodifiable-set-docs]: https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/Set.html#unmodifiable

## Instructions

Your nostalgia for Blorkemon™️ cards is showing no sign of slowing down, you even started collecting them again, and you
are getting your friends to join you.

In this exercise, you will use the `Set` interface to help you manage your collection, since duplicate cards are not
important when your goal is to get all existing cards.

## 1. Start a collection

You just found your old stash of Blorkemon™️ cards!
The stash contains a bunch of duplicate cards, so it's time to start a new collection by removing the duplicates.

You really want your friends to join your Blorkemon™️ madness, and the best way is to kickstart their collection by
giving them one card.

Implement the `newCollection` method, which transforms a list of cards into a `Set` representing your new collection.

```java
GottaSnatchEmAll.newCollection(List.of("Newthree", "Newthree", "Newthree"));
// => {"Newthree"}
```

## 2. Grow the collection

Once you have a collection, it takes a life of its own and must grow.

Implement the `addCard` method, which takes a new card and your current set of collected cards.
The method should add the new card to the collection if it isn't already present, and should return a `boolean`
indicating whether the collection was updated.

```java
Set<String> collection = GottaSnatchEmAll.newCollection("Newthree");
GottaSnatchEmAll.addCard("Scientuna",collection);
// => true

collection.contains("Scientuna");
// => true
```

## 3. Start trading

You really want your friends to join your Blorkemon™️ madness, so it's time to start trading!

When trading with friends not every trade is worth doing, or can be done at all.
You should only trade if both you and your friend have a card the other does not have.

Implement the `canTrade` method, that takes your current collection and the collection of one of your friends.
It should return a `boolean` indicating whether a trade is possible, following the rules above.

```java
Set<String> myCollection = Set.of("Newthree");
Set<String> theirCollection = Set.of("Scientuna");
GottaSnatchEmAll.canTrade(myCollection, theirCollection);
// => true
```

## 4. Identify common cards

You and your Blorkemon™️ enthusiast friends gather and wonder which cards are the most common.

Implement the `commonCards` method, which takes a list of collections and returns a collection of cards that all collections
have.

```java
GottaSnatchEmAll.commonCards(List.of(Set.of("Scientuna"), Set.of("Newthree","Scientuna")));
// => {"Scientuna"}
```

## 5. All of the cards

Do you and your friends collectively own all of the Blorkemon™️ cards?

Implement the `allCards` method, which takes a list of collections and returns a collection of all different cards in
all the collections combined.

```java
GottaSnatchEmAll.allCards(List.of(Set.of("Scientuna"), Set.of("Newthree","Scientuna")));
// => {"Newthree", "Scientuna"}
```

## Source

### Created by

- @sanderploegsma

### Contributed to by

- @kahgoh