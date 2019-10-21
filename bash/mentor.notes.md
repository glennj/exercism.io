You don't need to `echo -n` -- the command substitution automatically removes all trailing newlines.

You don't need to nest the arithmetic: this is OK
```bash
    if (( $1 % num == 0 )); then
```

Note that you don't need `$` for normal variables inside an arithmetic expression: more C-like arithmetic looks nicer.

Get out of the habit of using ALLCAPS variable names, leave those as reservied by the shell. One day you'll write `PATH=something` and the wonder why your script is broken.

You can use the `+=` concatenating assignment operator: `sound+=$(sound $num)`

For lines 17-21, consider the `${var:-default value}` form of [hell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion).
