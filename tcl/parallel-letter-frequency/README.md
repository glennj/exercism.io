# Parallel Letter Frequency

Count the frequency of letters in texts using parallel computation.

Parallelism is about doing things in parallel that can also be done
sequentially. A common example is counting the frequency of letters.
Create a function that returns the total frequency of each letter in a
list of texts and that employs parallelism.

## Threads

You'll implement parallelism with threads: subdivide the input into chunks
that code running in a thread can examine asynchronously. 

Some reading material for Tcl threads:

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
tclsh parallel-letter-frequency.test            # Will stop testing after the first failure.
RUN_ALL=1 tclsh parallel-letter-frequency.test  # Will run all tests and report all failures.
```

## Feedback, Issues, Pull Requests
The [exercism/tcl](https://github.com/exercism/tcl) repository on GitHub is
the home for all of the Tcl exercises on Exercism.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Source

