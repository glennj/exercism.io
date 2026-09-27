# Help

## Running the tests

Install dependencies.

```sh
dart pub get
```

Run the tests.

```sh
dart test
```

For more detailed per-test output, use the expanded reporter.

```sh
dart test -r expanded
```

Most exercises have multiple tests, with all but the first skipped by default.
To work through tests one at a time, change `skip: true` to `skip: false` on each test as you go.

Alternatively, run all tests at once without editing the file.

```sh
dart test --run-skipped
```

All tests are run by the online test runner on submission, including skipped ones.

## Submitting your solution

You can submit your solution using the `exercism submit lib/scrabble_score.dart` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [Dart track's documentation](https://exercism.org/docs/tracks/dart)
- The [Dart track's programming category on the forum](https://forum.exercism.org/c/programming/dart)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

To get help if you're having trouble, you can use one of the following resources:

- [Dart API Documentation](https://api.dart.dev/)
- [Dart Gitter Chat](https://gitter.im/dart-lang/home)
- [Community Information](https://www.dart.dev/community)
- [/r/dartlang](https://www.reddit.com/r/dartlang) is the Dart subreddit.
- [StackOverflow](https://stackoverflow.com/questions/tagged/dart) can be used to search for your problem and see if it has been answered already. You can also ask and answer questions.