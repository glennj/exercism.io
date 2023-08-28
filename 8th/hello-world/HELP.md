# Help

## Running the tests

If you have the 8th binary on your path, issue
```cmd
8th test.8th
```
on Windows, or on Linux
```bash
8th test.8th
```

If the 8th binary is not on the PATH, in Windows enter either
```cmd
\path\to\8th test.8th
```
or
```cmd
\path\to\8th -f test-words.8th -f <task-name>.8th -f <task-name>-tests.8th
```
Linux or OS/X, either
```bash
/path/to/8th test.8th
```
or 
```bash
/path/to/8th -f test-words.8th -f <task-name>.8th -f <task-name>-tests.8th
```

If the tests are not specified using the `-f` flag, the user will enter the CLI with the test-words and the `<task-name>` definition loaded.

## Submitting your solution

You can submit your solution using the `exercism submit hello-world.8th` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [8th track's documentation](https://exercism.org/docs/tracks/8th)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

Help from other 8th programmers can be found on the [8th forum](https://8th-dev.com/forum/).

Also on-line is the list of [8th words](https://8th-dev.com/words.html) and the [8th manual](https://8th-dev.com/manual.html).