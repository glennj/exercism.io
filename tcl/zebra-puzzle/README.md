# Zebra Puzzle

Welcome to Zebra Puzzle on Exercism's Tcl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Solve the zebra puzzle.

1. There are five houses.
2. The Englishman lives in the red house.
3. The Spaniard owns the dog.
4. Coffee is drunk in the green house.
5. The Ukrainian drinks tea.
6. The green house is immediately to the right of the ivory house.
7. The Old Gold smoker owns snails.
8. Kools are smoked in the yellow house.
9. Milk is drunk in the middle house.
10. The Norwegian lives in the first house.
11. The man who smokes Chesterfields lives in the house next to the man with the fox.
12. Kools are smoked in the house next to the house where the horse is kept.
13. The Lucky Strike smoker drinks orange juice.
14. The Japanese smokes Parliaments.
15. The Norwegian lives next to the blue house.

Each of the five houses is painted a different color, and their
inhabitants are of different national extractions, own different pets,
drink different beverages and smoke different brands of cigarettes.

Which of the residents drinks water?
Who owns the zebra?

This is the first exercise to include a pre-written package.
Note the presence of the file `pkgIndex.tcl` and the inclusion of the current directory in the `auto_path` variable.
These allow the `package require` command to be able to find the package's source file.

This is not a particularly well-written package: it's just some code that was borrowed from various sources.
The `interp alias` call is meant to hide the warts and make the permutations package easier to use.

References:
* [`auto_path`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/tclvars.htm)
* [`package`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/package.htm)
* [`pkg_mkIndex`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/pkgMkIndex.htm)

## Source

### Created by

- @glennj

### Contributed to by

- @sshine

### Based on

Wikipedia - http://en.wikipedia.org/wiki/Zebra_Puzzle