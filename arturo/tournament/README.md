# Tournament

Welcome to Tournament on Exercism's Arturo Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Tally the results of a small football competition.

Based on an input file containing which team played against which and what the outcome was, create a file with a table like this:

```text
Team                           | MP |  W |  D |  L |  P
Devastating Donkeys            |  3 |  2 |  1 |  0 |  7
Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6
Blithering Badgers             |  3 |  1 |  0 |  2 |  3
Courageous Californians        |  3 |  0 |  1 |  2 |  1
```

What do those abbreviations mean?

- MP: Matches Played
- W: Matches Won
- D: Matches Drawn (Tied)
- L: Matches Lost
- P: Points

A win earns a team 3 points.
A draw earns 1.
A loss earns 0.

The outcome is ordered by points, descending.
In case of a tie, teams are ordered alphabetically.

## Input

Your tallying program will receive input that looks like:

```text
Allegoric Alaskans;Blithering Badgers;win
Devastating Donkeys;Courageous Californians;draw
Devastating Donkeys;Allegoric Alaskans;win
Courageous Californians;Blithering Badgers;loss
Blithering Badgers;Devastating Donkeys;loss
Allegoric Alaskans;Courageous Californians;win
```

The result of the match refers to the first team listed.
So this line:

```text
Allegoric Alaskans;Blithering Badgers;win
```

means that the Allegoric Alaskans beat the Blithering Badgers.

This line:

```text
Courageous Californians;Blithering Badgers;loss
```

means that the Blithering Badgers beat the Courageous Californians.

And this line:

```text
Devastating Donkeys;Courageous Californians;draw
```

means that the Devastating Donkeys and Courageous Californians tied.

## Use of the medium vertical bar

~~~~exercism/caution
In Arturo, `|` is the pipe operator for reversing the default right to left order of function calls.
In v2 of the Unitt testing framework, there is currently an issue where instances of `|` are being unexpectedly evaluated inside strings.
Therefore, we've replaced `|` (vertical bar) with `❙` (medium vertical bar) in the expected outputs.
We're in the process of upgrading the track to Unitt v3 where this issue is fixed. Then we'll revert back to the expected `|` (vertical bar) delimiter.
~~~~

## Some notes about Arturo strings

You'll see some syntax in the tests that may be new to you: multiline strings enclosed in `{ braces }`.

### References

* [`:string` type][string] in the Language reference.
* [Basic Usage][usage] documentation in the Strings module.

[string]: https://arturo-lang.io/master/documentation/language/#string
[usage]: https://arturo-lang.io/master/documentation/library/strings/#basic-usage

### Arturo string literals

Arturo has a few ways to write string literals:

1. Single-line strings enclosed in double quotes

    ```arturo
    greeting: "Hello, World!"

    print express greeting     ; => "Hello, World!"
    ```

1. Line-ending strings using a "smart quote" `«`

    ```arturo
    farewell: « Goodbye, Mars!

    print express farewell     ; => "Goodbye, Mars!"
    ```

   These can be nicely readable if you have a list of short strings:

    ```arturo
    numberWords: [
        « one
        « two
        « three
        « four, and so on
    ]
 
    print express numberWords    ; => ["one" "two" "three" "four, and so on"]
    ```

1. Multi-line strings enclosed in braces.
   The "common" indentation will be removed.
   Leading and trailing newlines are removed.

    ```arturo
    multiline: {
        First line
        Second line
            Third line keeps indentation
        Last line!


    }

    print express first multiline   ; => 'F'
    print express last multiline    ; => '!'
    lines: split.lines multiline
    print size lines                ; => 4
    print express lines\2           ; => "    Third line keeps indentation"
    ```

1. Verbatim strings are preserved exactly, including leading/trailing whitespace

    ```arturo
    code: {:
        greet: function [name][
            print ~"Hello, |name|~"

        greet "World""
    :}

    print ">" ++ code ++ "<"
    print express first code
        ; => prints a literal newline character between single quotes
    print express (split.lines code | get 1)
        ; => "    greet: function [name]["
    ```

1. Heredoc-style strings reside below a line of 3 or more hyphens until the end of the file.
   This acts the same as Perl's `__DATA__` keyword.
   This style would be useful in a module, where you might store documentation or the license text in a variable at the end of the file.

    If this is in a file named "mymodule.art"

    ```arturo
    myLicense:
    ---------
    This text has been placed in the public domain.
    Do with it what you will.
    ```

    Then

    ```arturo
    import {mymodule}!
    print ~">|myLicense|<"  ; => >This text has been placed in the public domain.
                            ; => Do with it what you will.<
    ```

## Source

### Created by

- @glennj