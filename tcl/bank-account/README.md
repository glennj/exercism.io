# Bank Account

Simulate a bank account supporting opening/closing, withdrawals, and deposits
of money. Watch out for concurrent transactions!

A bank account can be accessed in multiple ways. Clients can make
deposits and withdrawals using the internet, mobile phones, etc. Shops
can charge against the account.

Create an account that can be accessed from multiple threads/processes
(terminology depends on your programming language).

It should be possible to close an account; operations against a closed
account must fail.

## Instructions

Run the test file, and fix each of the errors in turn. When you get the
first test to pass, go to the first pending or skipped test, and make
that pass as well. When all of the tests are passing, feel free to
submit.

Remember that passing code is just the first step. The goal is to work
towards a solution that is as readable and expressive as you can make
it.

Have fun!

## Threads

This exercises introduces threads to test how robust your bank account is 
for concurrent operations. Some reading material for Tcl threads:

* [Tcl Threading Model](https://www.tcl.tk/doc/howto/thread_model.html)
* [Threads Done Right… with Tcl](https://www.activestate.com/blog/threads-done-right-tcl/)
* [Multi-Threaded Tcl Scripts](http://www.beedub.com/book/4th/Threads.pdf),
  a chapter from _Practical Programming in Tcl and Tk, 4th Ed._,
copyright 2003 © Brent Welch, Ken Jones
[http://www.beedub.com/book/](http://www.beedub.com/book/)
    * Note, this chapter states "Most binary distributions of Tcl are not thread-enabled". This is no longer the case: Tcl distributions are now built with thread support (ref [TIP 364](https://core.tcl-lang.org/tips/doc/trunk/tip/364.md))
* [Thread Package Commands](https://tcl.tk/man/tcl8.6/ThreadCmd/contents.htm) from the Tcl documentation.



## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

## Running the tests
To run the test suite, execute one of the following commands:

```bash
tclsh bank-account.test            # Will stop testing after the first failure.
RUN_ALL=1 tclsh bank-account.test  # Will run all tests and report all failures.
```

## Feedback, Issues, Pull Requests
The [exercism/tcl](https://github.com/exercism/tcl) repository on GitHub is
the home for all of the Tcl exercises on Exercism.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Source

