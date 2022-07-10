# AWK notes

## Bookmarks

- [GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/html_node/index.html)
- [Built-in Funtions](https://www.gnu.org/software/gawk/manual/html_node/Built_002din.html)
- [Stack Overflow \[awk\] info page](https://stackoverflow.com/tags/awk/info)


### including files

gawk lets you pull in library files with the [`@include` directive][include].
There's also the `-i` command line option.

See what extra functionality ships with gawk: look at the `*.awk` files in
directories of the [AWKPATH environment variable][AWKPATH]:
```sh
gawk 'BEGIN {print ENVIRON["AWKPATH"]}'
```


[include]: https://www.gnu.org/software/gawk/manual/html_node/Include-Files.html
[AWKPATH]: https://www.gnu.org/software/gawk/manual/html_node/AWKPATH-Variable.html
