# Grade School

Welcome to Grade School on Exercism's Wren Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Instructions

Given students' names along with the grade that they are in, create a roster
for the school.

In the end, you should be able to:

- Add a student's name to the roster for a grade
  - "Add Jim to grade 2."
  - "OK."
- Get a list of all students enrolled in a grade
  - "Which students are in grade 2?"
  - "We've only got Jim just now."
- Get a sorted list of all students in all grades. Grades should sort
  as 1, 2, 3, etc., and students within a grade should be sorted
  alphabetically by name.
  - "Who all is enrolled in school right now?"
  - "Let me think. We have
    Anna, Barb, and Charlie in grade 1,
    Alex, Peter, and Zoe in grade 2
    and Jim in grade 5.
    So the answer is: Anna, Barb, Charlie, Alex, Peter, Zoe and Jim"

Note that all our students only have one name. (It's a small town, what
do you want?)

## For bonus points

Did you get the tests passing and the code clean? If you want to, these
are some additional things you could try:

- If you're working in a language with mutable data structures and your
  implementation allows outside code to mutate the school's internal DB
  directly, see if you can prevent this. Feel free to introduce additional
  tests.

Then please share your thoughts in a comment on the submission. Did this
experiment make the code better? Worse? Did you learn anything from it?

## Sorting Strings in Wren

Wren is still a very young language:
there is functionality, perhaps common in another languages, that is not yet implemented.
One such thing is the ability to sort strings.

Wren's List `sort` method uses the `<` comparison explicitly.
`String` does not implement `<(_)`.

One of the challenges of this exercise is implementing a way to sort a list of strings.


## References:

- [`sort(), sort(comparer)`][list-sort] in List
- [List module's `sort` implementation][list-module]


[list-sort]: https://wren.io/modules/core/list.html#sort(),-sort(comparer)
[list-module]: https://github.com/wren-lang/wren/blob/4ffe2ed38b238ff410e70654cbe38883f7533d3f/src/vm/wren_core.wren#L326

## Source

### Created by

- @glennj

### Based on

A pairing session with Phil Battos at gSchool - http://gschool.it