# Series

Welcome to Series on Exercism's MoonScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a string of digits, output all the contiguous substrings of length `n` in that string in the order that they appear.

For example, the string "49142" has the following 3-digit series:

- "491"
- "914"
- "142"

And the following 4-digit series:

- "4914"
- "9142"

And if you ask for a 6-digit series from a 5-digit string, you deserve whatever you get.

Note that these series are only required to occupy _adjacent positions_ in the input;
the digits need not be _numerically consecutive_.

## MoonScript-specific Instructions

This exercise requires you to implement the `slices` function as an **iterator**.
Iterators are described in the manual in "The generic for loop" under the [For Statement][lua-for] section.

There is a thorough tutorial at [Lua Iterators][tutorialspoint].


[lua-for]: https://www.lua.org/manual/5.4/manual.html#3.3.5
[tutorialspoint]: https://www.tutorialspoint.com/lua/lua_iterators.htm

## Source

### Created by

- @glennj

### Based on

A subset of the Problem 8 at Project Euler - https://projecteuler.net/problem=8