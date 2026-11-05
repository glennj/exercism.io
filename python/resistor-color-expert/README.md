# Resistor Color Expert

Welcome to Resistor Color Expert on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

If you want to build something using a Raspberry Pi, you'll probably use _resistors_.
Like the previous [`Resistor Color Duo`][resistor-color-duo-exercise] and [`Resistor Color Trio`][resistor-color-trio-exercise] exercises, you will be translating resistor color bands to human-readable labels.

- Each resistor has a resistance value.
- Resistors are small — so small in fact that if you printed the resistance value on them, it would be hard to read.
  To get around this problem, manufacturers print color-coded bands onto the resistors to denote their resistance values.
- Each band acts as a digit of a number.
  For example, if they printed a brown band (value 1) followed by a green band (value 5), it would translate to the number 15.


[resistor-color-duo-exercise]: https://exercism.org/tracks/python/exercises/resistor-color-duo
[resistor-color-trio-exercise]: https://exercism.org/tracks/python/exercises/resistor-color-trio

## Instructions

In this exercise, you are going to create a helpful program so that you don't have to remember the values of the bands.
The program will take 1, 4, or 5 colors as input and output the correct value in ohms.
The color bands are encoded as follows:

- black: 0
- brown: 1
- red: 2
- orange: 3
- yellow: 4
- green: 5
- blue: 6
- violet: 7
- grey: 8
- white: 9

In [`Resistor Color Trio`][resistor-color-trio-exercise] you decoded the first three color bands.
For instance: orange-orange-brown translated to the main value `330`.
In this exercise you will need to add _tolerance_ to the mix.
Tolerance is the maximum amount that a value can be above or below the main value.
For example, if the last band is green, the maximum tolerance will be `±0.5%`.

The tolerance band will have one of these values:

- grey - 0.05%
- violet - 0.1%
- blue - 0.25%
- green - 0.5%
- brown - 1%
- red - 2%
- gold - 5%
- silver - 10%

The four-band resistor is built up like this:

| Band_1  | Band_2  | Band_3     | Band_4    |
| ------- | ------- | ---------- | --------- |
| Value_1 | Value_2 | Multiplier | Tolerance |

Examples:

- orange-orange-brown-green would be `330` ohms with a `±0.5%` tolerance.
- orange-orange-red-grey would be `3300` ohms with a `±0.05%` tolerance.

The difference between a four and five-band resistor is that the five-band resistor has an extra band to indicate a more precise value.

| Band_1  | Band_2  | Band_3  | Band_4     | Band_5    |
| ------- | ------- | ------- | ---------- | --------- |
| Value_1 | Value_2 | Value_3 | Multiplier | Tolerance |

Examples:

- orange-orange-orange-black-green would be `333` ohms with a `±0.5%` tolerance.
- orange-red-orange-blue-violet would be `323 million` ohms with a `±0.1%` tolerance.

There are also one band resistors.
One band resistors only have the color black with a value of 0.


Your program should translate an input `list` of resistor band colors into a label:

"... ohms ±...%"

So an input `list` of `["orange", "orange", "black", "green"]` should return:

"33 ohms ±0.5%"

When there are thousands of ohms, we say "kiloohms".
That's similar to saying "kilometer" for 1000 meters, and "kilograms" for 1000 grams.

So an input `list` of `["orange", "orange", "orange", "grey"]` should return:

"33 kiloohms ±0.05%"

When there are millions of ohms, we say "megaohms".

So an input `list` of `["orange", "orange", "blue", "red"]` should return:

"33 megaohms ±2%"

[resistor-color-trio-exercise]: https://exercism.org/tracks/python/exercises/resistor-color-trio

## Source

### Created by

- @meatball133
- @bethanyg

### Contributed to by

- @yrahcaz7

### Based on

Based on earlier resistor color exercises made by Erik Schierboom and Maud de Vries - https://github.com/exercism/problem-specifications/issues/1464