# Nth Prime

Welcome to Nth Prime on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a number n, determine what the nth prime is.

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

If your language provides methods in the standard library to deal with prime numbers, pretend they don't exist and implement them yourself.

## The `--argjson` option

The tests use the `jq` option `--argjson n 1`.
This creates a jq variable named `$n` holding the _**number**_ value `1`.

Contrast this with the option `--arg n 1`
which creates a jq variable named `$n` holding the _**string**_ value `"1"`.

In your solution, use `$n` as the input to get the nth prime.

See [Invoking jq][man-invoke] in the manual.

[man-invoke]: https://stedolan.github.io/jq/manual/v1.6/#Invokingjq

## Source

### Created by

- @glennj

### Based on

A variation on Problem 7 at Project Euler - https://projecteuler.net/problem=7