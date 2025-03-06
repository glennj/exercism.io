# Matching Brackets

Welcome to Matching Brackets on Exercism's Tcl Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

You're given the opportunity to write software for the Bracketeer™, an ancient but powerful mainframe.
The software that runs on it is written in a proprietary language.
Much of its syntax is familiar, but you notice _lots_ of brackets, braces and parentheses.
Despite the Bracketeer™ being powerful, it lacks flexibility.
If the source code has any unbalanced brackets, braces or parentheses, the Bracketeer™ crashes and must be rebooted.
To avoid such a scenario, you start writing code that can verify that brackets, braces, and parentheses are balanced before attempting to run it on the Bracketeer™.

## Instructions

Given a string containing brackets `[]`, braces `{}`, parentheses `()`, or any combination thereof, verify that any and all pairs are matched and nested correctly.
Any other characters should be ignored.
For example, `"{what is (42)}?"` is balanced and `"[text}"` is not.

Tcl is a [very simple language](https://tcl.tk/man/tcl8.6/TclCmd/Tcl.htm),
but the way they interpreter parses code has a couple of nasty edge cases.

## Curly braces

Braces in Tcl are simply a way to quote a block of text -- that text may be
interpreted as code or data. Within braces, nested braces may appear but
the nested braces must be balanced. The Tcl man page says this:

> Braces nest within the word: for each additional open brace there must be
> an additional close brace (however, if an open brace or close brace within
> the word is _quoted with a backslash_ then it is not counted in locating the
> matching close brace).

The wording there is quite specific: if you want to have an unmatched open or close
brace character, it must be quoted with a backslash. So don't do this:
```tcl
proc isOpenBrace {char} {
    return [expr {$char eq "{"}]
    # ......................^ will not work
}
```
You must do this
```tcl
proc isOpenBrace {char} {
    return [expr {$char eq "\{"}]
    # ......................^^ will work
}
```

This can be problematic with the way Tcl parses comments, which is different
from most languages.  There is more discussion on the [Tcl
wiki](https://wiki.tcl-lang.org/page/Why+can+I+not+place+unmatched+braces+in+Tcl+comments).

## Square brackets

Similarly, since Tcl uses brackets for [Command
substitution.](https://tcl.tk/man/tcl8.6/TclCmd/Tcl.htm#M11), even within
double quoted strings, you have to be careful about escaping open brackets.

## Parentheses

With the exception of [associative array
variables](https://tcl.tk/man/tcl8.6/TclCmd/Tcl.htm#M12), parentheses are
simply ordinary characters.

## Source

### Created by

- @glennj

### Contributed to by

- @sshine

### Based on

Ginna Baker