# Help

## Running the tests

Testing on the MoonScript track is exactly like [testing on the Lua track][lua-tests].
From an exercise directory, enter:

```console
$ busted
```

## Pending tests

This track follows Exercism's [Test-Driven Development][tdd] methodology.
You are given a complete test suite, but only the first test is "active".

To enable the tests, change a test's command from `pending` to `it`.

For those of you that like to run all the tests all the time, you can use a command-line tool to change them all at once.
Here's an example using perl (which is installed just about everywhere):

```console
$ perl -i -pe 's/^\s+\Kpending\b/it/' two_fer_spec.moon
```

When you are working in the online editor, the test runner will automatically run all the tests.

[lua-tests]: https://exercism.org/docs/tracks/lua/tests
[tdd]: https://exercism.org/docs/using/solving-exercises/tdd

## Submitting your solution

You can submit your solution using the `exercism submit hello_world.moon` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [MoonScript track's documentation](https://exercism.org/docs/tracks/moonscript)
- The [MoonScript track's programming category on the forum](https://forum.exercism.org/c/programming/moonscript)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

If you're having trouble, try these resources:

- [Learning Resources](https://moonscript.org/#learning) on the MoonScript website.
- The [`moonscript` channel](https://discord.com/channels/454435414044966913/454435533792346142) on the MoonScript Discord.
- The [`get-help` channel](https://discord.com/channels/854117591135027261/1082698079163134073) on Exercism's Discord.

Make sure you read the [installation instructions](https://exercism.org/docs/tracks/moonscript/installation) so you are set up correctly.