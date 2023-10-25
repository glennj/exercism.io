# Pizza Pi

Welcome to Pizza Pi on Exercism's Common Lisp Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

In Common Lisp, like many languages, numbers come in a few of types – two of the most basic are:

## Integers

Like many languages Common Lisp contains integers.
These are whole numbers without a decimal point (like `-6`, `0`, `25`, `1234`, etc.)

Common Lisp defines no limits on the magnitude of integers.
Integers can be arbitrarily large (or small if negative).

In general, if you are working with only whole numbers, you should prefer integers as they don't suffer from the same loss of precision as floating-point numbers do over many calculations.

## Floating Point Numbers

Like many languages, Common Lisp contains floating point numbers.
These are fractional or whole numbers including a decimal point (like `3.14`, `-1.7`, `99.99`, `2048.0`).

## Arithmetic

Common Lisp uses the standard arithmetic operators for most operations but is somewhat unique in using a "prefix-notation" as  opposed to the more familiar "infix-notion".

More visually:

```lisp
;; Infix-notation (non-lisp languages)
1 + 2 + 3 + 4 + 5 ; => 15
;; Prefix-notation (lisp languages)
(+ 1 2 3 4 5)     ; => 15
```

While prefix notion turns some operations like `2 + 2` into the somewhat unfamiliar `(+ 2 2)` form, it makes it much easier to operate on more than one number at a time.

### Basic Math

Common Lisp has all the typical functions: `+`, `-`, `*`, `/` for addition, subtraction, multiplication and division.

When considering division one may want to know the remainder of a division, that is done by the `mod` (for modulo) function which takes a number and a divisor as arguments: 

```lisp
(mod 4 2)  ; => 0
(mod 2 4)  ; => 2
(mod 10 3) ; => 1
```

### Comparing Numbers

Finally, you may find it useful to compare different numbers using functions like `=` (equal), `/=` (not equal to), and `>=` (greater than or equal to).
When these comparisons are true (as in `(= 1 1)`), they return `T` and when they aren't (as in `(> 0 1)`), they return `NIL`.

## Instructions

Lilly the Lisp Alien loves cooking. She wants to throw a pizza party with her friends but, to make the most of her ingredients, she'd like to know exactly how much of each ingredient she'll need before starting.

To solve this, Lilly has drafted a program to automate some of the planning but needs your help finishing it.
Will you help Lilly throw the proportionally perfect pizza party?

## 1. A Dough Ratio

First things first, every great pizza starts with a delicious dough!

Lilly is a fan of thin, crispy pizzas with a thicker crust.
The dough needed for the middle is a constant 200g, but every 20cm of crust requires 45g of dough.

Thankfully, Lilly calculated the equation for grams of dough (g) from the number of pizzas (n) and their diameter (d):

`g = n * (((45 * pi * d) / 20) + 200)`

Lilly needs a function that:
* Takes the number of pizzas (n), their diameter (d)
* And returns the dough needed to the nearest gram (g).

For example, to make 4 pizzas 30cm in diameter:

```lisp
(dough-calculator 4 30) ; => 1648
```

## 2. A Splash of Sauce

Lilly is meticulous when applying her sauce, but the size of her pizzas can be inconsistent.

To figure out the diameter (d), she uses the below equation with sauce applied (s) as a variable:

`d = square-root of ((40 * s) / (3 * pi))`

Lilly needs a function that calculates the pizza diameter (d) from every milliliter of sauce applied (s).

For example, given Lilly has used 250ml of sauce:

```lisp
(size-from-sauce 250) ; => 32.57
```

## 3. Some Cheese, Please

On Lilly's planet, all cheese comes in perfect cubes and is sold by size.
(What were you expecting? This is an alien planet after all...)

Once again, Lilly calculated an equation for the number of pizzas (n) of some diameter (d) made from a cheese cube of side-length (l):

`n = (2 * (l^3)) / (3* pi * (d^2))`

Create a function that:
* Takes a side-length of a cheese cube (l) and the pizzas' diameter (d),
* And returns the number of pizzas (n) made while rounding down.

For example, given a 25x25x25cm cheese cube and pizzas 30cm in diameter:

```lisp
(pizzas-per-cube 25 30) ; => 3
```

## 4. A Fair Share

Finally, Lilly wants her pizzas to divide into 8 slices each and distributed evenly among her friends.

Create a function that:
* Takes a number of pizzas and number of friends
* and returns `T` if the pizzas will evenly distribute among friends and `NIL` otherwise.

For example:

```lisp
;; For splitting 3 pizzas between 4 friends
(fair-share-p 3 4) ; => T
;; For splitting 2 pizzas between 3 friends
(fair-share-p 2 3) ; => NIL
```

## Source

### Created by

- @TheLostLambda
- @verdammelt