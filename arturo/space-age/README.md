# Space Age

Welcome to Space Age on Exercism's Arturo Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

The year is 2525 and you've just embarked on a journey to visit all planets in the Solar System (Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus and Neptune).
The first stop is Mercury, where customs require you to fill out a form (bureaucracy is apparently _not_ Earth-specific).
As you hand over the form to the customs officer, they scrutinize it and frown.
"Do you _really_ expect me to believe you're just 50 years old?
You must be closer to 200 years old!"

Amused, you wait for the customs officer to start laughing, but they appear to be dead serious.
You realize that you've entered your age in _Earth years_, but the officer expected it in _Mercury years_!
As Mercury's orbital period around the sun is significantly shorter than Earth, you're actually a lot older in Mercury years.
After some quick calculations, you're able to provide your age in Mercury Years.
The customs officer smiles, satisfied, and waves you through.
You make a mental note to pre-calculate your planet-specific age _before_ future customs checks, to avoid such mix-ups.

~~~~exercism/note
If you're wondering why Pluto didn't make the cut, go watch [this YouTube video][pluto-video].

[pluto-video]: https://www.youtube.com/watch?v=Z_2gbGXzFbs
~~~~

## Instructions

Given an age in seconds, calculate how old someone would be on a planet in our Solar System.

One Earth year equals 365.25 Earth days, or 31,557,600 seconds.
If you were told someone was 1,000,000,000 seconds old, their age would be 31.69 Earth-years.

For the other planets, you have to account for their orbital period in Earth Years:

| Planet  | Orbital period in Earth Years |
| ------- | ----------------------------- |
| Mercury | 0.2408467                     |
| Venus   | 0.61519726                    |
| Earth   | 1.0                           |
| Mars    | 1.8808158                     |
| Jupiter | 11.862615                     |
| Saturn  | 29.447498                     |
| Uranus  | 84.016846                     |
| Neptune | 164.79132                     |

~~~~exercism/note
The actual length of one complete orbit of the Earth around the sun is closer to 365.256 days (1 sidereal year).
The Gregorian calendar has, on average, 365.2425 days.
While not entirely accurate, 365.25 is the value used in this exercise.
See [Year on Wikipedia][year] for more ways to measure a year.

[year]: https://en.wikipedia.org/wiki/Year#Summary
~~~~

In this exercise, you will be expected to return the ages using a [physical quantity] with a custom unit representing the length of a single year on a that planet.
In Arturo, a quantity consists of a numerical amount and an associated [unit of measurement][unit-of-measurement].

```arturo
length: 10`m
units length ; `m
scalar length ; 10

10`m * 10`m ; 100 m²
```

~~~~exercism/caution
Arturo's built-in `yr unit converts to 31556952 seconds.
Therefore, you'll need to define and use your own year unit in terms of the number of seconds referenced previously in the instructions.
~~~~

Quantities can be [converted][convert] to another compatible unit.
```arturo
convert 1`m `ft ; 1250/381 ft
convert 100`m2 `ft2 ; 156250000/145161 ft²

property `m  ; length
property `m2 ; area

convert 1`m `m2 ; conversion error results
```

~~~~exercism/note
Because of the precision limitations with floating point numbers, Arturo sometimes stores the scalar as a rational number.
The `to` function can be used to retrieve the amount instead as an approximate floating point number.

```arturo
length: convert 1`m `ft ; 1250/381 ft
to :floating length ; 3.280839895013123
```
~~~~

Arturo also allows us to [specify] our own units for quantities.

```arturo
specify 'nauMile 1.151`mi
property `nauMile length
specify 'lea 3.00579`nauMile
property `lea ; length
depth: 20000`lea
convert depth `km ; 111355.7993425152 km
```

[physical-quantities]: https://en.wikipedia.org/wiki/Physical_quantity  
[unit-of-measurement]: https://en.wikipedia.org/wiki/Unit_of_measurement
[convert]: https://arturo-lang.io/master/documentation/library/quantities/convert/
[specify]: https://arturo-lang.io/master/documentation/library/quantities/specify/

## Source

### Created by

- @BNAndras

### Based on

Partially inspired by Chapter 1 in Chris Pine's online Learn to Program tutorial. - https://pine.fm/LearnToProgram/?Chapter=01