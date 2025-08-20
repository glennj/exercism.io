# Help

## Running the tests

Each exercise contains a test file.
Run the tests using the `bats` program.

```bash
bats hello_world.bats
```

`bats` will need to be installed.
See the [Testing on the Bash track][tests] page for instructions to install `bats` for your system.

[tests]: https://exercism.org/docs/tracks/bash/tests

## Help for assert functions

The tests use functions from the [bats-assert][bats-assert] library.
Help for the various `assert*` functions can be found there.

[bats-assert]: https://github.com/bats-core/bats-assert

## Debugging output

```exercism/caution
This works locally with `bats`, but **not** in the Exercism online editor.
```

When running tests, `bats` captures both stdout and stderr for comparison with the expected output.
If you print debug messages to stdout (`echo`) or stderr (`>&2`), they will be included in the captured output and may cause the test to fail.

To print debug information without affecting the test results, `bats` provides file descriptor **3** for this purpose.
Anything redirected to `>&3` will be shown during the test run but will not be included in the captured output used for assertions.

Example:

```bash
#!/usr/bin/env bash

# This debug message will not interfere with test output comparison
echo "debug message" >&3

# Normal program output (this is what your tests will see and compare)
echo "Hello, World!"
```

Example run:

```none
$ bats hello_world.bats
hello_world.bats
 ✓ Say Hi!
debug message
1 test, 0 failures
```

This allows you to see helpful debug output without affecting the tests.

## Skipped tests

Solving an exercise means making all its tests pass.
By default, only one test (the first one) is executed when you run the tests.
This is intentional, as it allows you to focus on just making that one test pass.
Once it passes, you can enable the next test by commenting out or removing the next annotation:

```bash
[[ $BATS_RUN_SKIPPED == true ]] || skip
```

## Overriding skips

To run all tests, including the ones with `skip` annotations, you can run:

```bash
BATS_RUN_SKIPPED=true bats exercise_name.bats
```

It can be convenient to use a wrapper function to save on typing:

```bash
bats() {
    BATS_RUN_SKIPPED=true command bats *.bats
}
```

Then run tests with just:

```bash
bats
```

## Submitting your solution

You can submit your solution using the `exercism submit house.sh` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [Bash track's documentation](https://exercism.org/docs/tracks/bash)
- The [Bash track's programming category on the forum](https://forum.exercism.org/c/programming/bash)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

Check your code for syntax errors: paste your code into
[https://shellcheck.net](https://shellcheck.net) (or [install it](https://github.com/koalaman/shellcheck#user-content-installing) on your machine).

Stack Overflow will be your first stop for bash questions.

* start with the [`bash` tag](https://stackoverflow.com/questions/tagged/bash) to search for your specific question: it's probably already been asked
* under the bash tag on Stackoverflow, the [Learn more...](https://stackoverflow.com/tags/bash/info) link has _tons_ of good information.
    * the "Books and Resources" section is particularly useful.
* the [`bash` tag](https://unix.stackexchange.com/questions/tagged/bash) on Unix & Linux is also active

## External utilities

`bash` is a language to write "scripts" -- programs that can call
external tools, such as
[`sed`](https://www.gnu.org/software/sed/),
[`awk`](https://www.gnu.org/software/gawk/),
[`date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
and even programs written in other programming languages, 
like [`Python`](https://www.python.org/).
This track does not restrict the usage of these utilities, and as long
as your solution is portable between systems and does not require
installation of third party applications, feel free to use them to solve
the exercise.

For an extra challenge, if you would like to have a better understanding of
the language, try to re-implement the solution in pure bash, without using
any external tools. There are some types of problems that bash cannot solve,
such as floating point arithmetic and manipulating dates: for those, you
must call out to an external tool.