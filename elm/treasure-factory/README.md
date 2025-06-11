# Treasure Factory

Welcome to Treasure Factory on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Phantom Types

Elm types allow generic types, that generally add flexibility to an interface.
For example, the type `Maybe a` can hold a value of any type.

```elm
type Maybe a = Nothing | Just a
```

Note that on the example above, the type parameter `a` is used on both sides of the equal sign `=`.
We say that `a` is bound to some data inside the type definition.

### The phantom type technique

In certain cases however, the type parameter only appears on the left side of the equation.

```elm
type Distance unit = Distance Float
```

In the definition of `Distance` above, `unit` is a free type parameter, not bound to any data in the type.
We can also call this type a **phantom type**.

It is surprisingly useful when we want to enforce constraints at compile time.
For example, we want to make sure that we only add distances of the same unit.

```elm
-- Distance is an opaque type, since the module does not expose its variants.
-- This means that users may only use the functions meter, foot and add to manipulate distances.
module Distance exposing (Distance, Meter, Foot, meter, foot, add)

-- The Distance type has a phantom type 'unit'.
type Distance unit = Distance Float

-- We define two types that will be used in place
-- of the phantom type 'unit' in our constructor functions.
type Meter = Meter
type Foot = Foot

-- Constructor for Meter
meter : Distance Meter
meter = Distance 1.0

-- Constructor for Foot
foot : Distance Foot
foot = Distance 0.3048

-- The add function cannot take two parameters of different types.
-- So we cannot add meters and feet by mistake.
add : Distance unit -> Distance unit -> Distance unit
add (Distance d1) (Distance d2) = Distance (d1 + d2)
```

Code outside of that module cannot access the internal of the `Distance` type.
Users can add two `Distance Meter` but cannot add a `Distance Meter` with a `Distance Foot`.

```elm
import Distance exposing (Distance)

-- Compiles
twoMeters = Distance.add Distance.meter Distance.meter

-- Does not compile
errDist = Distance.add Distance.meter Distance.foot
```

### Extensible records as phantom types

There is no restriction on which types can be phantom, and record types can be used as well.
When used as phantom types, records are able to express complex constraints that can transform through functions.

Let's add a new distance unit inside of the `Distance` module called `LegoBlock`.
Physical blocks, being regulated objects, always have two properties: they have non-fractional and non-negative distances.

```elm
type LegoBlock = LegoBlock

fourStuds : Distance { properties | unit: LegoBlock, nonFractional : (), nonNegative: () }
fourStuds = Distance 4.0
```

`nonFractional` and `nonNegative` are the fields of a record that doesn't exist outside of a type argument, so it's fitting to give them the type `()`, called the __unit type__, which holds no information beyond the fact that it is there.

Obtaining arbitrary `LegoBlock` distances is of course possible, for example after computing distance differences or ratios.

```elm
negativeStud : Distance { properties | unit: LegoBlock, nonFractional : () }
negativeStud = Distance -1.0

threeFiddyStud : Distance { properties | unit: LegoBlock, nonNegative : () }
threeFiddyStud = Distance 3.50

crazyStud : Distance { properties | unit: LegoBlock }
crazyStud = Distance -13.37
```

All the above values are valid and will compile, however we have a special interest in values like `fourStuds` that have both the `nonFractional` and `nonNegative` properties because they represent physical blocks that can be combined with

```elm
combineLegoBlocks
  :  Distance { properties | unit: LegoBlock, nonFractional : (), nonNegative: () }
  -> Distance { properties | unit: LegoBlock, nonFractional : (), nonNegative: () }
  -> Distance { properties | unit: LegoBlock, nonFractional : (), nonNegative: () }
combineLegoBlocks = add
```

Let's add some functions to let users create and refine `LegoBlock` distances.

```elm
newLegoBlock : Float -> Distance { properties | unit: LegoBlock }
newLegoBlock dist = Distance dist

floorDistance : Distance properties -> Distance { properties | nonFractional : () }
floorDistance (Distance dist) = Distance (toFloat (floor dist))

ceilingDistance : Distance properties -> Distance { properties | nonFractional : () }
ceilingDistance (Distance dist) = Distance (toFloat (ceiling dist))

absDistance : Distance properties -> Distance { properties | nonNegative : () }
absDistance (Distance dist) = Distance (abs dist)
```

Note that `floorDistance`, `ceilingDistance` and `absDistance` can handle unit other than `LegoBlock`, and in general do not make any assumptions on the input properties, they merely guarantee that the output will have a specific property, either `nonFractional` or `nonNegative`.

Let's look at some outcomes.

```elm
import Distance exposing (Distance)

-- Compiles
distance1 = combineLegoBlocks fourStuds fourStuds

-- Does not compile
distance2 = combineLegoBlocks fourStuds threeFiddyStud

-- Compiles
distance3 = combineLegoBlocks fourStuds (floorDistance threeFiddyStud)

-- Does not compile
distance4 = combineLegoBlocks fourStuds negativeStud

-- Compiles
distance5 = combineLegoBlocks fourStuds (absDistance negativeStud)

-- Does not compile
distance6 = combineLegoBlocks fourStuds crazyStud

-- Compiles
distance7 = combineLegoBlocks fourStuds (floorDistance (absDistance crazyStud))

-- Compiles
distance8 = combineLegoBlocks fourStuds (ceilingDistance (absDistance crazyStud))
```

In general, `floorDistance` and `ceilingDistance` provide different results, but the same guarantees.
This is the strength of the phantom type technique: providing flexible choices to users while maintaining strong guarantees.

## Instructions

You, the Master of Evil, take great pride in the quality of the `TreasureChest`s found in your Evil dungeons all over the world.
It's not easy to keep up the quality, your dungeon managers keep messing up treasure making, so you decide to provide an Elm API to bend them to your Will.

There are two conditions on which you will not budge:
1. a `TreasureChest` should be protected by a secure password of at least 8 characters
2. in a specific dungeon, each `TreasureChest` should hold a unique treasure

Your dungeon managers will come up with a list of password/treasure suggestions, and only suitable `TreasureChest`s will be created.

With these criterion, there are two possible ways of creating secure chests from a list of suggestions: 
1. remove the ones with insecure passwords, then remove the ones with duplicate treasures
2. remove the ones with duplicate treasures, then remove the ones with insecure passwords

Those two might not give the same results (for example for `[("strong_password", GoldStatue), ("1234", GoldStatue)]`), but you don't mind either way, so you want to let the dungeon managers decide.

An API that leaves the users some choice, but still guarantees properties of the final result?
Sounds like a perfect fit for the phantom type technique!

## 1. Provide a placeholder for treasure chests

The `TreasureChest` type and its companion `getTreasure` are already given, but you need to come up with a type for a chest suggestion.

Implement the `Chest` type, implement `makeChest` and fix the type signatures of `secureChest` and `uniqueTreasures`.
The type signatures of `makeChest` and `makeTreasureChest` are already provided as part of the requirements, do not modify them.

A `Chest` should hold the same data as a `TreasureChest` and should have two type arguments, one for `treasure` and a record phantom type for `conditions`.
Note that because `Chest` is using a phantom type, and should therefore be opaque to prevent its use anywhere outside of the `TreasureFactory` module.
In this case, it is not even exposed at all.

Edit the type signatures of `secureChest` and `uniqueTreasures` to add the constraints using extensible records as phantom types.
`secureChest` should take a `Chest` without specific conditions and return a `Maybe Chest`, with the extra condition `securePassword : ()` in its phantom record.
`uniqueTreasures` should take a `List Chest` without specific conditions and return a `List Chest`, with the extra condition `uniqueTreasure : ()` added to the phantom record.

~~~~exercism/note
Elm tests have access to exposed functions, but not to type signatures, it is therefore impossible for tests to verify that you are using the right signatures.
Of course, the strongest indication that you have the right signatures is to succeed in getting the module and tests to compile and run, but we have created an analyzer rule that will check your type signatures once you submit your solution.
~~~~

## 2. Select secure chests

Once you have chests ready, you need to pick out the secure ones.

Implement `secureChest` that only returns a `Just` variant for chests with a password of 8 or more characters.
The `Chest` that fulfill that conditions should have an extra `securePassword : ()` condition added to their phantom type.

## 3. Select unique treasures

Only the rarest of treasures should be allowed in a dungeon, even one copy makes a treasure look cheap.

Implement `uniqueTreasures` that takes a list of `Chest` and returns a list of the `Chest` that have a unique treasure in the input list.
If a treasure appears twice in the input, it shouldn't be in the output.
The input `Chest`s should not have specific conditions and output ones should have the extra condition `uniqueTreasure : ()` added.

## 4. Enjoy the final product

Enjoy the best of `TreasureChest`s that will be sure to attract adventurers like flies to honey.

Implement `makeTreasureChest` that takes a `Chest` that is both secure and unique and creates a `TreasureChest`.
Since `TreasureChest` is an opaque type, this will be the only way to create one, even lousy dungeon managers won't be able to mess it up.

## Source

### Created by

- @jiegillet

### Contributed to by

- @ceddlyburge