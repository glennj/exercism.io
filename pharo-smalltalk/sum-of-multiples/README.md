# Sum of Multiples

Welcome to Sum of Multiples on Exercism's Pharo Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a list of factors and a limit, add up all the unique multiples of the factors that are less than the limit.
All inputs will be greater than or equal to zero.

## Example

Suppose the limit is 20 and the list of factors is [3, 5].
We need to find the sum of all unique multiples of 3 and 5 that are less than 20.

Multiples of 3 less than 20: 3, 6, 9, 12, 15, 18
Multiples of 5 less than 20: 5, 10, 15

The unique multiples are: 3, 5, 6, 9, 10, 12, 15, 18

The sum of the unique multiples is: 3 + 5 + 6 + 9 + 10 + 12 + 15 + 18 = 78

So, the answer is 78.

The trick to this exercise understanding how to iterate accross a range of numbers as well as check a Collection for any multiples that are divisible by a number. Try using the Spotter to look up potential classes and browse the categories of methods in those classes. Also remember that the Pharo environment is extensively cross referenced. You can right click on any class or method and browse "references" to a class (to see how it might be created), as well as "senders" of a method (to see how it might be used).

## Source

### Created by

- @macta

### Contributed to by

- @bencoman

### Based on

A variation on Problem 1 at Project Euler - http://projecteuler.net/problem=1