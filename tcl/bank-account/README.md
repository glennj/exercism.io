# Bank Account

Welcome to Bank Account on Exercism's Tcl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Simulate a bank account supporting opening/closing, withdrawals, and deposits of money.
Watch out for concurrent transactions!

A bank account can be accessed in multiple ways.
Clients can make deposits and withdrawals using the internet, mobile phones, etc.
Shops can charge against the account.

Create an account that can be accessed from multiple threads/processes (terminology depends on your programming language).

It should be possible to close an account; operations against a closed account must fail.

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

## Installation

Some Tcl distributions do not include Tcl's Thread package by default: it may need to be install separately.
Verify with
```bash
$ echo 'package require Thread' | tclsh
```
If that produces _no output_ then the Thread package is already installed.

Execute `apt install tcl-thread` (or `dnf install tcl-thread`) as required.

## Source

### Created by

- @glennj

### Contributed to by

- @sshine