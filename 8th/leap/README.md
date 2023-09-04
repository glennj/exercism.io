# Leap

Welcome to Leap on Exercism's 8th Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a year, report if it is a leap year.

The tricky thing here is that a leap year in the Gregorian calendar occurs:

```text
on every year that is evenly divisible by 4
  except every year that is evenly divisible by 100
    unless the year is also evenly divisible by 400
```

For example, 1997 is not a leap year, but 1996 is.
1900 is not a leap year, but 2000 is.

## Notes

Though our exercise adopts some very simple rules, there is more to learn!

For a delightful, four minute explanation of the whole leap year phenomenon, go watch [this youtube video][video].

[video]: https://www.youtube.com/watch?v=xX96xng7sAE

## Running and testing your solutions
 
### From the command line
 
Simply type `8th test.8th`. It is assumed that the 8th binary is declared in the PATH environment variable.
 
### From a REPL
 
Start 8th loading test-words.8th: `8th -f test-words.8th`.
This will start a CLI session where, once you’ve written some code in your solution file, you can load it with `"leap.8th" f:include`
and you can run the tests with `"leap-tests.8th" f:include` or you can copy and paste a single test from that file or enter your own. 
 
### Editing the leap-tests.8th file
 
This is encouraged at the beginning of your journey. Insert a back-slash space before all but the first one or two tests. Write your code to solve the first test or two. Then progressively enable more tests until you can pass all of them.

## Source

### Created by

- @axtens

### Based on

CodeRanch Cattle Drive, Assignment 3 - https://coderanch.com/t/718816/Leap