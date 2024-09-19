# Help

## Running the tests

To test your solution simply run:

```bash
make test
```

This will run the test file `<exercise-name>-test.ys` which contains all of the
tests that you will need to pass for your solution to be correct.
You should study the test file to understand exactly what your solution needs
to do.

> Note: The first time you run `make test` it will also make sure that the
> correct version of the `ys` YAMLScript interpreter is installed in the
> correct place.

If you run `make test` before changing any files, all the tests will fail.
This is the proper and expected behavior.
The normal exercise development process is to iteratively improve your
solution, running `make test` after each change, until all the tests pass.

If you want to only run a specific test (while concentrating on that part of the
solution), you can add this key/value pair to that test:

```yaml
  ONLY: true
```

If you want to skip particular tests (while working on other parts of the
solution), you can add this key/value pair to those tests:

```yaml
  SKIP: true
```


## Testing Prerequisites

YAMLScript currently runs on MacOS and Linux.

Your computer will need to have the following very common commands installed:

* `bash` - You can run the tests under any shell, but `bash` must be available.
* `make` - You must use GNU `make` to run the tests.
  In some environments tt might have a different name than `make`.
  Possibly `gmake`.
* `prove` - This program is part of any normal Perl installation.
* `curl` - Used to install ys if it is not already installed.

It is extremely likely that you already have all of these programs installed.

## Submitting your solution

You can submit your solution using the `exercism submit darts.ys` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [YAMLScript track's documentation](https://exercism.org/docs/tracks/yamlscript)
- The [YAMLScript track's programming category on the forum](https://forum.exercism.org/c/programming/yamlscript)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

If you need help with this YAMLScript exercise, try the following resources:

* [YAMLScript Documentation](https://yamlscript.org/doc/)
* [YAMLScript Examples - Rosetta Code](
  https://rosettacode.org/wiki/Category:YAMLScript#mw-pages)
* [YAMLScript Matrix Chat](https://matrix.to/#/#chat-yamlscript:yaml.io)
* [YAMLScript Slack Chat](https://clojurians.slack.com/messages/C05HQFMTURF)
* [YAMLScript IRC Chat](https://web.libera.chat/?channel=yamlscript)
* [YAMLScript GitHub Discussions](
  https://github.com/yaml/yamlscript/discussions)
* [YAMLScript on Stack Overflow](
  https://stackoverflow.com/questions/tagged/yamlscript)