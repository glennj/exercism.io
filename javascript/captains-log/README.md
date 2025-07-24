# Captain's Log

Welcome to Captain's Log on Exercism's JavaScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

Many programs need (pseudo-)random values to simulate real-world events.

Common, familiar examples include:

- A coin toss: a random value from ('H', 'T').
- The roll of a die: a random integer from 1 to 6.
- Shuffling a deck of cards: a random ordering of a card list.
- The creation of trees and bushes in a 3-D graphics simulation.

Generating truly random values with a computer is a [surprisingly difficult technical challenge][why-randomness-is-hard], which is why there are also "pseudorandom" generators.

<!-- prettier-ignore -->
~~~exercism/caution
The `Math.random()` function should NOT be used for security and cryptographic applications!
Finish the learning exercise(s) about this concept to learn more
~~~

## Generating random numbers

In Javascript, you can generate psuedorandom numbers using the [`Math.random()`][Math.random] function.
It will return a psuedorandom floating-point number between 0 (inclusive), and 1 (exclusive).

[why-randomness-is-hard]: https://www.malwarebytes.com/blog/news/2013/09/in-computers-are-random-numbers-really-random
[Math.random]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random

## Instructions

## Instructions

Mary is a big fan of the TV series Star Trek: The Next Generation.
She often plays pen-and-paper role playing games, where she and her friends pretend to be the crew of the Starship Enterprise.
Mary's character is Captain Picard, which means she has to keep the captain's log.
She loves the creative part of the game, but doesn't like to generate random data on the spot.

Help Mary by creating random generators for data commonly appearing in the captain's log.

### 1. Generate a random starship registry number

Enterprise (registry number NCC-1701) is not the only starship flying around!
When it rendezvous with another starship, Mary needs to log the registry number of that starship.

Registry numbers start with the prefix "NCC-" and then use a number from 1000 to 9999 (both inclusive).

Implement the `randomShipRegistryNumber()` function that returns a random starship registry number.

```javascript
randomShipRegistryNumber();
// => "NCC-1947"
```

### 2. Generate a random stardate

What's the use of a log if it doesn't include dates?

A stardate is a floating point number.
The adventures of the Starship Enterprise from the first season of The Next Generation take place between the stardates 41000.0 and 42000.0.
The "4" stands for the 24th century, the "1" for the first season.

Implement the function `randomStardate` that returns a floating point number between 41000.0 (inclusive) and 42000.0 (exclusive).

```javascript
randomStardate();
// => 41458.15721310934
```

### 3. Generate a random planet

The Starship Enterprise encounters many planets in its travels.
Planets in the Star Trek universe are split into categories based on their properties.
For example, Earth is a class M planet.
All possible planetary classes are: D, H, J, K, L, M, N, R, T, and Y.

Implement the `randomPlanetClass()` function.
It should return one of the planetary classes at random.

```javascript
randomPlanetClass();
// => "K"
```

## Source

### Created by

- @SneakyMallard

### Contributed to by

- @SleeplessByte