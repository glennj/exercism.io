# Booking up for Beauty

Welcome to Booking up for Beauty on Exercism's Perl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Packages and Modules

### Importing

A module is usually loaded with the `use` keyword.
This will both load and call the `import` method on it, importing any exported symbols.

Symbols in the package's `@EXPORT` array will be imported by default.
Symbols in the package's `@EXPORT_OK` array must be imported explicitly.

### Exporting

For a symbol to be imported, it must first be exported.
The [Exporter][Exporter] module is included with Perl and is one of the most convenient options for exporting.

Defining the module:
```perl
package Foo;

use Exporter ('import');
our @EXPORT_OK = ('bar');

sub bar { ... }
```

Using the module:
```perl
use Foo ('bar');

bar();
```

### Core Modules

Along with [Exporter][Exporter], Perl has a large variety of [core modules][perl-core-modules] available to use.
Some examples useful for exercises you may encounter here include [List::Util][List::Util], [Time::Piece][Time::Piece], and [Math::BigRat][Math::BigRat].

### CPAN

In additon to core modules, [CPAN][metacpan] has thousands of additional modules created by the Perl community available for installation.
The Perl track uses a [cpanfile][test-runner-cpanfile] to install a selection of modules which can be used with Exercism's Perl test runner.

[perl-core-modules]: https://perldoc.pl/modules
[metacpan]: https://metacpan.org/
[test-runner-cpanfile]: https://github.com/exercism/perl5-test-runner/blob/main/cpanfile
[Exporter]: https://perldoc.pl/Exporter
[List::Util]: https://perldoc.pl/List::Util
[Time::Piece]: https://perldoc.pl/Time::Piece
[Math::BigRat]: https://perldoc.pl/Math::BigRat

## Instructions

In this exercise you'll be working on an appointment scheduler for a beauty salon.

You have three tasks, which will all involve appointment dates.

## 1. Check if an appointment has already passed

Implement the `appointment_has_passed` subroutine that takes a date string and checks if the appointment was somewhere in the past:

```perl
appointment_has_passed("2019-07-25T13:45:00")
// => true
```

Date strings will be passed in ISO 8601 datetime format: `YYYY-mm-ddTHH:MM:SS`

You will need to implement the private `_parse_datetime` subroutine.
A couple of modules are suggested for you:

- [Time::Piece][time-piece] is a core Perl module.
- [DateTime::Tiny][datetime-tiny] is available from CPAN.

## 2. Check if appointment is in the afternoon

Implement the `is_afternoon_appointment` function that takes a date string and checks if the appointment is in the afternoon (>= 12:00 and < 18:00):

```perl
is_afternoon_appointment("2019-07-25T13:45:00")
// => true
```

## 3. Describe the time and date of the appointment

Implement the `describe_appointment` function that takes a date string and returns a description of that date and time:

```perl
describe_appointment("2019-07-25T13:45:00")
// => "You have an appointment on 07/25/2019 1:45 PM"
```

Note the hour is in the range 0-12 and does not have a leading zero or space.

[time-piece]: https://perldoc.pl/Time::Piece
[datetime-tiny]: https://metacpan.org/pod/DateTime::Tiny

## Source

### Created by

- @m-dango