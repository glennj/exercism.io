# AWK notes

## Bookmarks

- [GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/html_node/index.html)
- [Built-in Funtions](https://www.gnu.org/software/gawk/manual/html_node/Built_002din.html)
- [Stack Overflow \[awk\] info page](https://stackoverflow.com/tags/awk/info)


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


---

[include]: https://www.gnu.org/software/gawk/manual/html_node/Include-Files.html
[AWKPATH]: https://www.gnu.org/software/gawk/manual/html_node/AWKPATH-Variable.html
[join]: https://www.gnu.org/software/gawk/manual/html_node/Join-Function.html
