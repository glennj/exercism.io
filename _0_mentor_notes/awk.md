# AWK notes

## Bookmarks

- [GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/html_node/index.html)
- [Built-in Funtions](https://www.gnu.org/software/gawk/manual/html_node/Built_002din.html)
- [Stack Overflow \[awk\] info page](https://stackoverflow.com/tags/awk/info)

## Syllabus

The AWK syllabus is a work-in-progress.
But a couple of the chapters are online:

* [Fundamentals](https://exercism.org/tracks/awk/concepts/fundamentals)
* [More about Patterns](https://exercism.org/tracks/awk/concepts/patterns)
* [Numbers and Strings](https://exercism.org/tracks/awk/concepts/nums-strs)

## Fields

In awk, think of `$` as an _operator_: "get me the value in the field referenced by the next thing"

* `$1` -- the value of field 1
* `$NF` -- the value of the field number contained in the NF variable
    * similarly: `x = 10; print $x` prints the value of field 10.
    * you can put expressions after `$` as well: `print $(NF - 1)` is the 2nd-last field
* `$0` -- special, the current record.

See [Fields](https://www.gnu.org/software/gawk/manual/html_node/Fields.html) in the manual.

## Truth Values

A note about "truthiness" in awk: 
* the number 0 is false; any other number is true,
* the empty string is false; any other string is true

https://www.gnu.org/software/gawk/manual/html_node/Truth-Values.html#index-truth-values

## including files

gawk lets you pull in library files with the [`@include` directive][include].
There's also the `-i` command line option.

See what extra functionality ships with gawk: look at the `*.awk` files in
directories of the [AWKPATH environment variable][AWKPATH]:
```sh
gawk 'BEGIN {print ENVIRON["AWKPATH"]}'
```

## Joining arrays

as you work through the track, you'll experiment with various ways to join arrays.
Here's one that ships with gawk: [https://www.gnu.org/software/gawk/manual/html_node/Join-Function.html][join]
```sh
gawk '
  @include "join"
  BEGIN {
    n = split("foo bar baz", a)
    print join(a, 1, n, "::")
}' foo bar baz
```
```none
foo::bar::baz
```

## Reversing an array

@citizen428 has [an ingenious reverse strategy](https://exercism.org/tracks/awk/exercises/secret-handshake/solutions/citizen428):
```awk
PROCINFO["sorted_in"] = and($1, 16) ? "@ind_num_desc" : "@ind_num_asc"
for (i in handshake) ...
```

## Variables are global

All AWK variables are global in scope, with one exception: 
function parameters are scoped to the function.

```awk
function myfunc(a) {
    b = 20
    print "in myfunc, a=" a " and b=" b
}
BEGIN {
    a = 1
    b = 2
    print "before, a=" a " and b=" b
    myfunc(10)
    print "after, a=" a " and b=" b
}
```
```sh
$ gawk -f scope_demo.awk </dev/null
before, a=1 and b=2
in myfunc, a=10 and b=20
after, a=1 and b=20
```

---

All AWK variables are global in scope, with one exception:
function parameters are scoped to the function.
This means, if you have variables in your function that should be "local",
put them in the argument list.

```awk
function add(a, b,    total) {
    total = a + b
    return total
}
BEGIN {
    print add(3, 4)
    print "total is:", total
}
```
outputs:
```none
7
total is:
```
The BEGIN scope cannot see the `total` variable from the function.

It is customary to separate "required" args from "local" args with 4 or more spaces.

AWK doesn't mind if you pass it fewer arguments than appear in the args list.
Those arguments are undefined until you define them.

## Exercises

<!-- ------------------------------------- -->

### two-fer

A more awk-ish way to approach this:
* set the variable to the default value in the BEGIN block.
* for each subsequent line, if the line is not empty then overwrite the variable with the contents of the line.
* in the END block, print the output.

### raindrops

The last thing I'd like to mention is the divisors 3, 5, 7. You use those (correctly IMO) as the indexes in the drops array. But then they are hardcoded again in the for loop on line 10

awk has a looping mechanism to iterate over the indexes of an array:
```awk
for (index in array) {...}
```

<details><summary>That may or may not iterate over the indexes in the the correct order; click for reasons:</summary>
  
---
  
Keep in mind that awk only has one kind of array, the associative array. The default iteration order is undefined (this is common to many programming languages).

GNU awk lets you control the iteration order in several ways
* order by _index_, numerically or alphabetically, increasing or decreasing
* order by _value_, numerically or alphabetically, increasing or decreasing
* with a custom sorting function

More details at [Using Predefined Array Scanning Orders with gawk](https://www.gnu.org/software/gawk/manual/html_node/Controlling-Array-Traversal.html) in the manual.
</details>


---

[include]: https://www.gnu.org/software/gawk/manual/html_node/Include-Files.html
[AWKPATH]: https://www.gnu.org/software/gawk/manual/html_node/AWKPATH-Variable.html
[join]: https://www.gnu.org/software/gawk/manual/html_node/Join-Function.html
