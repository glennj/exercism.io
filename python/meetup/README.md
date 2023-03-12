# Meetup

Welcome to Meetup on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Recurring monthly meetups are generally scheduled on the given weekday of a given week each month.
In this exercise you will be given the recurring schedule, along with a month and year, and then asked to find the exact date of the meetup.

For example a meetup might be scheduled on the _first Monday_ of every month.
You might then be asked to find the date that this meetup will happen in January 2018.
In other words, you need to determine the date of the first Monday of January 2018.

Similarly, you might be asked to find:

- the third Tuesday of August 2019 (August 20, 2019)
- the teenth Wednesday of May 2020 (May 13, 2020)
- the fourth Sunday of July 2021 (July 25, 2021)
- the last Thursday of November 2022 (November 24, 2022)

The descriptors you are expected to process are: `first`, `second`, `third`, `fourth`, `last`, `teenth`.

Note that descriptor `teenth` is a made-up word.

It refers to the seven numbers that end in '-teen' in English: 13, 14, 15, 16, 17, 18, and 19.
But general descriptions of dates use ordinal numbers, e.g. the _first_ Monday, the _third_ Tuesday.

For the numbers ending in '-teen', that becomes:

- 13th (thirteenth)
- 14th (fourteenth)
- 15th (fifteenth)
- 16th (sixteenth)
- 17th (seventeenth)
- 18th (eighteenth)
- 19th (nineteenth)

So there are seven numbers ending in '-teen'.
And there are also seven weekdays (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday).
Therefore, it is guaranteed that each day of the week (Monday, Tuesday, ...) will have exactly one numbered day ending with "teen" each month.

If asked to find the teenth Saturday of August, 1953 (or, alternately the "Saturteenth" of August, 1953), we need to look at the calendar for August 1953:

```plaintext
    August 1953
Su Mo Tu We Th Fr Sa
                   1
 2  3  4  5  6  7  8
 9 10 11 12 13 14 15
16 17 18 19 20 21 22
23 24 25 26 27 28 29
30 31
```

The Saturday that has a number ending in '-teen' is August 15, 1953.

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