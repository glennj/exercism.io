# Help

## Running the tests

The Arturo track uses the [Unitt unit-testing package][unitt] for all exercise tests.
This package provides a testng framework supporting both modern RSpec-inspired syntax and legacy XUnit-inspired syntax.
Arturo will automatically download and install this package when you run the tests, requiring minimal setup.

To begin running tests, you can use `arturo tester.art` or `exercism test` within the exercise folder.
Only the first test will be run and reported back to you.
Once that test passes, unskip the next one and run the tests again until each test passes.
For both the RSpec and XUnit setups, that's as simple as removing `.skip` from `it.skip` or `test.skip` respectively.
Once all tests pass, submit your solution, but be aware that the test runner runs all tests, even the skipped ones.

## Getting Started

Your downloaded exercise folder will have some files, but these are the three essential ones.

```
leap/
├── src/leap.art         # Your solution file
├── tests/test-leap.art  # Test cases for the exercise
└── tester.art           # Test runner entry point
```

Running `arturo tester.art` will set up Unitt and then run the tests inside `tests/test-leap.art`.
The tests will import `src/leap.art` which contains your solution and tben make a series of assertions about what your solution should return for specific inputs.


## Solution File

```arturo
isLeap?: function [year][
    panic "please implement the isLeap? function"
]
```

When starting an exercise, your solution file comes with a template containing the basic structure of what's expected.
In this example from `Leap`, the file contains a barebones definition of the expected `isLeap?` function and notably a `panic` that stops test execution.
Remove the `panic` line and then begin coding your implementation for `isLeap?`.


## RSpec Test Structure

```arturo
import.version:2.0.1 {unitt}!
import {src/leap}!

describe "Leap" [
    it "a year not divisible by 4 is a common year" [
        expects.be:'false? @[isLeap? 2015]
    ]

    it.skip "a year divisible by 4 and not divisible by 100 is a leap year" [
        expects.be:'true? @[isLeap? 1996]
    ]
]
```

* `describe` defines a block of test cases validating something in common. These can be nested.
* `it` and `it.skip` both define a single test case which is either run or skipped respectively.
* `expects` introduces the assertion that is at the heart of a test case. `expects.be` takes a predicate function which is used to make the assertion.

In `expects.be:'false? @[isLeap? 2015]`, the test suite is making an assertion that the function `isLeap?` should return `false` for the year 2015.
Another way to write that is `expects.be:'equal? @[false isLeapYear? 2015]` where we're asserting the returned vaue of `isLeapYear? 2015` is equal to `false`.
This longer form comparing two values is the most common type of assertion on the Arturo track.
The expected value will always come first in the block before the result value.
Notably, `express <value>` is used sometimes to add quotes at the beginning and end of strings, like `expects.be:'equal? @[express "reward" express reverseString "drawer"]`.
To help readability, this will typically be split across multiple lines like this:
```arturo
expects.be:'equal? @[
    express "reward"
    express reverseString "drawer"
]
```

## XUnit Test Structure

```arturo
suite "Leap" [
    test "a year not divisible by 4 is a common year" [
        result: isLeap? 2015
        assert -> false = result
    ]

    test.skip "a year divisible by 4 and not divisible by 100 is a leap year" [
        result: isLeap? 1996
        assert -> true = result
    ]
]
```

The XUnit-like API introduced in v1 of Unitt is supported for backwards compatibility and is broadly equivalent to the modern RSpec-like API except for its less flexible assertions.

* `suite` defines a block of test cases validating something in common. These can be nested.
* `test` and `test.skip` both define a single test case which is either run or skipped respectively.
* `assert` introduces the assertion that is at the heart of a test case. The expression on the right side of the following `->` must be true.

With XUnit tests, typically variables representing the result and optionally the expected value are created before the `assert` line.

## Test Output

Whether you're using the RSpec or XUnit API, the test output will be broadly similar.
The only difference will be what's printed beneath each test name.
XUnit reports the assertion using the evaluated values so you'd see `false = false`.
RSpec, however, reports it as `false? false` because we used the `false?` function with `expects`.

Starting at the beginning, Arturo will report an error if the `panic` from the starting file hasm't been removed and not evaluate any tests.

```
===== tests/test-leap.art =====

Description: Leap 
 

══╡ Program Error ╞═════════════════════════════════════════════════════════════════════════════════════════════════ <script> ══

  please implement the isLeap? function
```

Once the `panic` is replaced, Arturo will report the status for each assertion made within a test.
Now the first test passes, but the second test is skipped.

```plaintext
===== tests/test-leap.art =====

Description: Leap 
 
    ✅ - assert that a year not divisible by 4 is a common year
         ✅: false? false

    ⏩ - assert that a year divisible by 4 and not divisible by 100 is a leap year
         skipped!



===== Statistics =====

 ⏏️   TOTAL: 1 assertions
✅  PASSED: 1 assertions
⏩ SKIPPED: 1 assertions
❌  FAILED: 0 assertions

===== ========== =====
```

After the second test is manually unskipped, both tests will be run.
However, my code only returns `false` so the second test fails its assertion.

```plaintext
===== tests/test-leap.art =====

Description: Leap 
 
    ✅ - assert that a year not divisible by 4 is a common year
         ✅: false? false

    ❌ - assert that a year divisible by 4 and not divisible by 100 is a leap year
         ❌: true? false



===== Statistics =====

 ⏏️   TOTAL: 2 assertions
✅  PASSED: 1 assertions
⏩ SKIPPED: 0 assertions
❌  FAILED: 1 assertions

===== ========== =====
```

This process will continue as you unskip more tests.
Once all tests pass, you're finished with the exercise and can submit the solution.
Please note that the test runner will run all tests whether they're skipped or not localy.

[unitt]: https://unitt.pkgr.art/

## Submitting your solution

You can submit your solution using the `exercism submit src/line-up.art` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [Arturo track's documentation](https://exercism.org/docs/tracks/arturo)
- The [Arturo track's programming category on the forum](https://forum.exercism.org/c/programming/arturo)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

Here are some resources for getting help if you run into trouble

* [Official Documentation][official-docs] covers syntax, language features, and the standard library modules.
* [Arturo Discord][discord] is Arturo's official Discord server.
* [Rosetta Code][rosetta-code] has over 770+ Arturo code examples for many different tasks.

[official-docs]: https://arturo-lang.io/master/documentation/
[discord]: https://discord.gg/YdVK2CB
[rosetta-code]: https://rosettacode.org/wiki/Category:Arturo