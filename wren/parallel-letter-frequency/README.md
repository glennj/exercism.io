# Parallel Letter Frequency

Welcome to Parallel Letter Frequency on Exercism's Wren Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Count the frequency of letters in texts using parallel computation.

Parallelism is about doing things in parallel that can also be done sequentially.
A common example is counting the frequency of letters.
Employ parallelism to calculate the total frequency of each letter in a list of texts.

## Concurency vs. Parallelism

Wren provides _concurrency_ via a construct called _fibers_.
From the Wren manual:

> Fibers are a bit like threads except they are cooperatively scheduled.
> That means Wren doesn’t pause one fiber and switch to another until you tell it to.

Wren's execution is single-threaded so we don't actually get _parallelism_.

Here's a quick definition for each that illustrates the diferences between the two:

- Concurrency is when two or more tasks can start, run and complete in overlapping time periods, being executed by the same processing unit.
- Parallelism is when two or more tasks can start and run at the same time, being executed independently of eachother by separate processing units.

Read more about [concurrency in Wren][concurrency].

[concurrency]: https://wren.io/concurrency.html

## Source

### Created by

- @glennj