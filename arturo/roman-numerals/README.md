# Roman Numerals

Welcome to Roman Numerals on Exercism's Arturo Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

Today, most people in the world use Arabic numerals (0–9).
But if you travelled back two thousand years, you'd find that most Europeans were using Roman numerals instead.

To write a Roman numeral we use the following Latin letters, each of which has a value:

| M    | D   | C   | L   | X   | V   | I   |
| ---- | --- | --- | --- | --- | --- | --- |
| 1000 | 500 | 100 | 50  | 10  | 5   | 1   |

A Roman numeral is a sequence of these letters, and its value is the sum of the letters' values.
For example, `XVIII` has the value 18 (`10 + 5 + 1 + 1 + 1 = 18`).

There's one rule that makes things trickier though, and that's that **the same letter cannot be used more than three times in succession**.
That means that we can't express numbers such as 4 with the seemingly natural `IIII`.
Instead, for those numbers, we use a subtraction method between two letters.
So we think of `4` not as `1 + 1 + 1 + 1` but instead as `5 - 1`.
And slightly confusingly to our modern thinking, we write the smaller number first.
This applies only in the following cases: 4 (`IV`), 9 (`IX`), 40 (`XL`), 90 (`XC`), 400 (`CD`) and 900 (`CM`).

Order matters in Roman numerals!
Letters (and the special compounds above) must be ordered by decreasing value from left to right.

Here are some examples:

```text
 105 => CV
---- => --
 100 => C
+  5 =>  V
```

```text
 106 => CVI
---- => --
 100 => C
+  5 =>  V
+  1 =>   I
```

```text
 104 => CIV
---- => ---
 100 => C
+  4 =>  IV
```

And a final more complex example:

```text
 1996 => MCMXCVI
----- => -------
 1000 => M
+ 900 =>  CM
+  90 =>    XC
+   5 =>      V
+   1 =>       I
```

## Instructions

Your task is to convert a number from Arabic numerals to Roman numerals.

For this exercise, we are only concerned about traditional Roman numerals, in which the largest number is MMMCMXCIX (or 3,999).

~~~~exercism/note
There are lots of different ways to convert between Arabic and Roman numerals.
We recommend taking a naive approach first to familiarise yourself with the concept of Roman numerals and then search for more efficient methods.

Make sure to check out our Deep Dive video at the end to explore the different approaches you can take!
~~~~

## Arturo instructions

For this exercise, you'll need to support two different ways of calling the `stringify` word:

1. With the `roman` attribute (e.g. `stringify.roman 3999`)
2. Without the `roman` attribute (e.g. `stringify 3999`)

For more information, check the [attributes][attributes] documentation as well as the [`attr`][attr] documentation.

~~~~exercism/caution
In addition to `attr`, the `attrs` function is useful: it returns all the attributes of the function call as a dictionary.

Beware that these two functions are destructive!

The implementation of Arturo uses an ["attributes table"][createAttrsStack].

* `attrs` [explicitly empties the table][getAttrsDict] after retrieving the attributes.
* `attr` [removes ("pops") the attribute from the table][builtinAttr].

An example:

```arturo
showAttributes: function [x][
    print attr 'question
    print attrs
    print attrs
]

showAttributes .question:"6 * 9" .answer:42 'arg
```
outputs
```
6 * 9
[answer:42]
[]
```

At each step, we see the attribute dictionary shrink.

**Conclusion**: be aware that you can only fetch attributes once.
If you need to refer back to attributes, capture them at the start of your functions.

[getAttrsDict]: https://github.com/arturo-lang/arturo/blob/2ef0081570feb6ab752404c32bbf85132df379fb/src/vm/stack.nim#L187
[builtinAttr]: https://github.com/arturo-lang/arturo/blob/2ef0081570feb6ab752404c32bbf85132df379fb/src/library/Reflection.nim#L85
[createAttrsStack]: https://github.com/arturo-lang/arturo/blob/2ef0081570feb6ab752404c32bbf85132df379fb/src/vm/stack.nim#L136
~~~~

[attributes]: https://arturo-lang.io/master/documentation/language/#attributes
[attr]: https://arturo-lang.io/master/documentation/library/reflection/attr/

## Source

### Created by

- @erikschierboom

### Based on

The Roman Numeral Kata - https://codingdojo.org/kata/RomanNumerals/