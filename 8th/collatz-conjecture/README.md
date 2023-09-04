# Collatz Conjecture

Welcome to Collatz Conjecture on Exercism's 8th Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

The Collatz Conjecture or 3x+1 problem can be summarized as follows:

Take any positive integer n.
If n is even, divide n by 2 to get n / 2.
If n is odd, multiply n by 3 and add 1 to get 3n + 1.
Repeat the process indefinitely.
The conjecture states that no matter which number you start with, you will always reach 1 eventually.

Given a number n, return the number of steps required to reach 1.

## Examples

Starting with n = 12, the steps would be as follows:

0. 12
1. 6
2. 3
3. 10
4. 5
5. 16
6. 8
7. 4
8. 2
9. 1

Resulting in 9 steps.
So for input n = 12, the return value would be 9.

## Running and testing your solutions
 
### From the command line
 
Simply type `8th test.8th`. It is assumed that the 8th binary is declared in the PATH environment variable.
 
### From a REPL
 
Start 8th loading test-words.8th: `8th -f test-words.8th`.
This will start a CLI session where, once you’ve written some code in your solution file, you can load it with `"collatz-conjecture.8th" f:include`
and you can run the tests with `"collatz-conjecture-tests.8th" f:include` or you can copy and paste a single test from that file or enter your own. 
 
### Editing the collatz-conjecture-tests.8th file
 
This is encouraged at the beginning of your journey. Insert a back-slash space before all but the first one or two tests. Write your code to solve the first test or two. Then progressively enable more tests until you can pass all of them.

## Source

### Created by

- @axtens

### Based on

An unsolved problem in mathematics named after mathematician Lothar Collatz - https://en.wikipedia.org/wiki/3x_%2B_1_problem