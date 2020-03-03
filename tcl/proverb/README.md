# Proverb

For want of a horseshoe nail, a kingdom was lost, or so the saying goes.

Given a list of inputs, generate the relevant proverb. For example, given the list `["nail", "shoe", "horse", "rider", "message", "battle", "kingdom"]`, you will output the full text of this proverbial rhyme:

```text
For want of a nail the shoe was lost.
For want of a shoe the horse was lost.
For want of a horse the rider was lost.
For want of a rider the message was lost.
For want of a message the battle was lost.
For want of a battle the kingdom was lost.
And all for the want of a nail.
```

Note that the list of inputs may vary; your solution should be able to handle lists of arbitrary length and content. No line of the output text should be a static, unchanging string; all should vary according to the input given.


## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

## Running the tests
To run the test suite, execute one of the following commands:

```bash
tclsh proverb.test            # Will stop testing after the first failure.
RUN_ALL=1 tclsh proverb.test  # Will run all tests and report all failures.
```

## Feedback, Issues, Pull Requests
The [exercism/tcl](https://github.com/exercism/tcl) repository on GitHub is
the home for all of the Tcl exercises on Exercism.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Source

Wikipedia [http://en.wikipedia.org/wiki/For_Want_of_a_Nail](http://en.wikipedia.org/wiki/For_Want_of_a_Nail)

