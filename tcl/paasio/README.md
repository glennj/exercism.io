# PaaS I/O

Welcome to PaaS I/O on Exercism's Tcl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Report network IO statistics.

You are writing a [PaaS][paas], and you need a way to bill customers based on network and filesystem usage.

Create a wrapper for network connections and files that can report IO statistics.
The wrapper must report:

- The total number of bytes read/written.
- The total number of read/write operations.

[paas]: https://en.wikipedia.org/wiki/Platform_as_a_service

## Tcl-specific instructions

Tcl allows you to apply layers of "transformations" to open channels.
These transformations allow you to manipulate the incoming/outgoing data in any way.

References:

* The original [proposal][tip230] for channel transformations.
* The [`chan push`][chan] command documentation.
* The [`transchan`][transchan] documentation describing the API for channel transforms.

[tip230]: https://core.tcl-lang.org/tips/doc/trunk/tip/230.md
[transchan]: https://www.tcl-lang.org/man/tcl9.0/TclCmd/transchan.html
[chan]: https://www.tcl-lang.org/man/tcl9.0/TclCmd/chan.html#M33

## Source

### Created by

- @glennj

### Based on

Brian Matsuo - https://github.com/bmatsuo