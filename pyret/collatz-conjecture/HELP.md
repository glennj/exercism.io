# Help

## Running the tests

This track uses `pyret` which is [available as a NPM package][pyret-npm]. Once installed, you can run your exercise's test file by passing `pyret` its location relative to your current working directory.

Linux/MacOS

```bash
cd {path/to/exercise-folder-location}
```

Windows

```powershell
PS C:\Users\foobar> cd {path\to\exercise-folder-location}
```

Then you can run the tests by running the `pyret` command as shown below, replacing `{exercise-test.arr}` with the name of the test file.

Linux/MacOS

```bash
$ pyret {exercise-test.arr}
2/2 modules compiled ({exercise-test.arr})
Cleaning up and generating standalone...
Looks shipshape, all 9 tests passed, mate!
```

Windows

```powershell
PS C:\Users\foobar\Exercism\pyret\exercise> pyret {exercise-test.arr}
2/2 modules compiled ({exercise-test.arr})
Cleaning up and generating standalone...
Looks shipshape, all 9 tests passed, mate!
```

[pyret-npm]: https://www.npmjs.com/package/pyret-npm

## Submitting your solution

You can submit your solution using the `exercism submit collatz-conjecture.arr` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [Pyret track's documentation](https://exercism.org/docs/tracks/pyret)
- The [Pyret track's programming category on the forum](https://forum.exercism.org/c/programming/pyret)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

Below are some resources for getting help if you run into trouble:

* [Official Pyret Documentation]
* [Pyret Discuss on Google Groups]
* [Pyret Discord]
* [Pyret Twitter]

[Official Pyret Documentation]: https://pyret.org/docs/latest/
[Pyret Discuss on Google Groups]: https://groups.google.com/g/pyret-discuss
[Pyret Discord]: https://discord.gg/9nFHuaBp
[Pyret Twitter]: https://twitter.com/pyretlang