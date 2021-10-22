# Hangman

Welcome to Hangman on Exercism's Tcl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Implement the logic of the hangman game using functional reactive programming.

[Hangman][] is a simple word guessing game.

[Functional Reactive Programming][frp] is a way to write interactive
programs. It differs from the usual perspective in that instead of
saying "when the button is pressed increment the counter", you write
"the value of the counter is the sum of the number of times the button
is pressed."

Implement the basic logic behind hangman using functional reactive
programming.  You'll need to install an FRP library for this, this will
be described in the language/track specific files of the exercise.

[Hangman]: https://en.wikipedia.org/wiki/Hangman_%28game%29
[frp]: https://en.wikipedia.org/wiki/Functional_reactive_programming

## New approach to the exercise

Please ignore the part regarding an FRP library, a third party library is
not required for this exercise.  This exercise focuses on event-driven
programming using callbacks and Tcl's event loop.

You are required to construct a TCP server and a domain-specific protocol.
This server is launched with the word to guess.  The test script implements
the client that sends commands to the server.

### Domain-specific protocol

The protocol commands to implement are:

- `STATUS` to return the current state of the game. The response will be a
  three-elememt list:
    1) the number of bad guesses remaining,
    2) the word masked with underscores for unguessed letters, and
    3) a status field (one of "ongoing", "win"
  or "lose")
- `GUESS x` where `x` is a letter to guess. The server will respond with the
  status list after each guess.
- `SHUTDOWN` to remotely shutdown the server. The server will otherwise
  exit after a win or loss.

The server should be able to handle these commands case-insensitively.

### What port does the server use?

Start your server using **port number 0**: Tcl will find an unused port, so
we don't have to hardcode a port number in the test script.  All
interpreters running in a process (including threads) share the same
environment, so the server can communicate its port back to the test script
using the global `env` array. There's an example in the initial hangman.tcl
file.

### Hints

You'll need these Tcl commands:
[`socket`](https://www.tcl-lang.org/man/tcl8.6/TclCmd/socket.htm),
[`fileevent`](https://www.tcl-lang.org/man/tcl8.6/TclCmd/fileevent.htm),
[`vwait`](https://www.tcl-lang.org/man/tcl8.6/TclCmd/vwait.htm).

This [example socket program](https://www.tcl-lang.org/about/netserver.html) can get you started.

## Source

### Created by

- @glennj