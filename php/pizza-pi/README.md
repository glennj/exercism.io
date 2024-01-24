# Pizza Pi

Welcome to Pizza Pi on Exercism's PHP Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Integers

Integers are whole numbers that belong to the set `{..., -2, -1, 0, 1, 2, ...}`.
The maximum and minimum values of an integer is determined by the system PHP runs in.
Typically for most modern systems, the integer is a 64-bit value -- meaning it can represent `-9_223_372_036_854_775_808` to `9_223_372_036_854_775_807` inclusive.

```php
<?php

$a = 1234;
$a = 1_234_567;
```

You can write integer literal values in other than base 10 using a base prefix:

```php
<?php

$a = 0123; // octal number (equivalent to 83 decimal)
$a = 0o123; // octal number (as of PHP 8.1.0)
$a = 0x1A; // hexadecimal number (equivalent to 26 decimal)
$a = 0b11111111; // binary number (equivalent to 255 decimal)
```

> Taken from PHP's [documentation][syntax]

## Floating Point Numbers

Floating point numbers are used in PHP to represent a subset of real numbers.
They start with one or more digits separated by a decimal separator.

```php
<?php

$a = 1.234; 
$b = 1.2e3; 
$c = 7E-10;
$d = 1_234.567; // as of PHP 7.4.0
```

### Common pitfalls

Not all numbers and arithmetic operations can be performed with floating point numbers.
Using floating point numbers to represent dollars and cents can lead to rounding errors.

```php
<?php

$result = 0.1 + 0.2; // => 0.30000000000000004
```

## Arithmetic Operators

PHP provides a number of operators for performing arithmetic operations. PHP follows the standard mathematical order of operations for its arithmetic operators. The operators that are provided by PHP are:

* Identity (+)
* Negation (-)
* Addition (+)
* Subtraction (-)
* Multiplication (*)
* Division (/)
* Modulo (%)
* Exponentiation (**)

```php
$moles = +'10';
$aLotOfMoles = 6.022 * 10**23 * $moles;
```

[syntax]: https://www.php.net/manual/en/language.types.integer.php#language.types.integer.syntax

## Instructions

Lilly loves cooking.
She wants to throw a pizza party with her friends.
To make the most of her ingredients, she needs to know how much of each ingredient she requires before starting.

To solve this, Lilly has drafted a program to automate some of the planning but needs your help finishing it.
Will you help Lilly throw the proportionally perfect pizza party?

## 1. A Dough Ratio

Lilly is a fan of thin, crispy pizzas with a thinner crust.
The dough needed for the middle is a minimum 200g, but every person it serves requires another 20g of dough.

`grams = pizzas * ((persons * 20) + 200)`

Lilly needs a function that:
* Takes the number of pizzas
* The number of persons each pizza will serve 
* And returns the dough needed to the nearest gram.

For example, to make 4 pizzas that feed 8 people:

```php
<?php

$pizza_pi = new PizzaPi();
$pizza_pi->calculateDoughRequirement(4, 8);
// => 1440
```

## 2. A Splash of Sauce

Lilly is meticulous when applying her sauce, but the size of her pizzas can be inconsistent.
From her experience, she knows that it takes 125mL of sauce per pizza.
Lilly needs a function that calculates how many cans of sauce to buy.

`cans of sauce = pizzas * sauce per pizza / sauce can volume`

For example, given Lilly needs to make 8 pizzas, and each can is 250mL:

```php
<?php

$pizza_pi = new PizzaPi();
$pizza_pi->calculateSauceRequirement(8, 250);
// => 4
```

## 3. Some Cheese, Please

Cheese comes in perfect cubes and is sold by size.
Lilly calculated an equation to determine how many pizzas can  of some diameter (d) made from a cheese cube of side-length (l):

`pizzas = (cheese_dimension^3) / (thickness * PI * diameter)`

Create a function that:
* Takes a side-length dimension of a cheese cube
* Takes the desired thickness of the cheese layer
* Takes the diameter of the pizza
* And returns the number of pizzas that can be made while rounding down.

For example, given a 25x25x25cm cheese cube, 0.5cm thick cheese layer and pizzas 30cm in diameter:

```php
<?php

$pizza_pi = new PizzaPi();
$pizza_pi->calculateCheeseCubeCoverage(25, 0.5, 30);
// => 331
```

## 4. A Fair Share

Finally, Lilly wants her pizzas to divide into 8 slices each and distributed evenly among her friends.

Create a function that:
* Takes a number of pizzas and number of friends
* and returns the number of slices that will be left over if each person takes an equal number of slices.

For example:

```php
<?php

$pizza_pi = new PizzaPi();
$pizza_pi->calculateLeftOverSlices(2, 4);
// => 0
$pizza_pi->calculateLeftOverSlices(4, 3);
// => 2
```

## Source

### Created by

- @neenjaw