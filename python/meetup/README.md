# Meetup

Welcome to Meetup on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

In this exercise, you will be given a general description of a meetup date and then asked to find the actual meetup date.

Examples of general descriptions are:

- First Monday of January 2022
- Third Tuesday of August 2021
- Teenth Wednesday of May 2022
- Teenth Sunday of July 2021
- Last Thursday of November 2021

The descriptors you are expected to process are: `first`, `second`, `third`, `fourth`, `fifth`, `last`, `teenth`.

Note that descriptor `teenth` is a made-up word.
There are exactly seven numbered days in a month that end with "teenth" ("thirteenth" to "nineteenth").
Therefore, it is guaranteed that each day of the week (Monday, Tuesday, ...) will have exactly one numbered day ending with "teenth" each month.

For example, if given "First Monday of January 2022", the correct meetup date is January 3, 2022.

## Customizing and Raising Exceptions

Sometimes it is necessary to both [customize](https://docs.python.org/3/tutorial/errors.html#user-defined-exceptions) and [`raise`](https://docs.python.org/3/tutorial/errors.html#raising-exceptions) exceptions in your code. When you do this, you should always include a **meaningful error message** to indicate what the source of the error is. This makes your code more readable and helps significantly with debugging.

Custom exceptions can be created through new exception classes (see [`classes`](https://docs.python.org/3/tutorial/classes.html#tut-classes) for more detail.) that are typically subclasses of [`Exception`](https://docs.python.org/3/library/exceptions.html#Exception).

For situations where you know the error source will be a derivative of a certain exception type, you can choose to inherit from one of the [`built in error types`](https://docs.python.org/3/library/exceptions.html#base-classes) under the _Exception_ class. When raising the error, you should still include a meaningful message.

This particular exercise requires that you create a _custom exception_ to be [raised](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement)/"thrown" when your `meetup()` function is given a weekday name and number combination that is invalid. The tests will only pass if you customize appropriate exceptions, `raise` those exceptions, and include appropriate error messages.

To customize a `built-in exception`, create a `class` that inherits from that exception. When raising the custom exception with a message, write the message as an argument to the `exception` type:

```python
# subclassing the built-in ValueError to create MeetupDayException
class MeetupDayException(ValueError):
    """Exception raised when the Meetup weekday and count do not result in a valid date.

    message: explanation of the error.

    """
    def __init__(self, message):
        self.message = message

        
# raising a MeetupDayException
raise MeetupDayException("That day does not exist.")
```

## Source

### Created by

- @sjakobi

### Contributed to by

- @acedrew
- @behrtam
- @BethanyG
- @cmccandless
- @Dog
- @dvermd
- @ikhadykin
- @kytrinyx
- @N-Parsons
- @Peque
- @pheanex
- @tqa236
- @ZacharyRSmith

### Based on

Jeremy Hinegardner mentioned a Boulder meetup that happens on the Wednesteenth of every month - https://twitter.com/copiousfreetime