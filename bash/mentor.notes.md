## Arithmetic

<!-- -->

bash can do arithmetic, you don't need to call out to `bc`. See [Arithmetic Expansion](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion) in the manual.

<!-- -->

You don't need to nest the arithmetic: this is OK
```bash
    # instead of this
    if (( $(( $1 % num )) == 0 )); then
    # do this
    if (( $1 % num == 0 )); then
```

<!-- -->

Note that you don't need `$` for normal variables inside an arithmetic expression: more C-like arithmetic looks nicer.

## Variables

For lines 17-21, consider the `${var:-default value}` form of [Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion).

<!-- -->

Get out of the habit of using ALLCAPS variable names, leave those as reservied by the shell. One day you'll write `PATH=something` and then wonder why your script is broken.

## Assignment

You can use the `+=` concatenating assignment operator: these are equivalent:
```bash
foo=${foo}bar
foo+=bar
```

## Conditionals

In bash, prefer `[[...]]` over `[...]`. The double bracket conditional command gives you more features (including regular expression matching), and fewer surprises (particularly about unquoted variables).

For example, consider this:
```bash
if [ -z $1 ]; then exit 1; fi
```
What happens when $1 is unset or an empty string: bash will substitute the `$1` parameter with an empty string. Then bash will have this command:
```bash
if [ -z  ]; then exit 1; fi
```
The `[` and `[[` commands do their comparisons based on *the number of arguments given*. When there is only a single argument (excluding the mandatory closing `]` or `]]`) then the comparison is "true" if the argument is non-empty. Here there is a single argument "-z" and it is non-empty, so it returns "true". This test does execute the desired `exit 1`, but not for the reason you might think. 

<!-- -->

Note that the `==` operator in bash is not a string equality operator, it is a _pattern matching_ operator. So this may give surprising results:
```bash
target="?"
for char in {a..z}; do
    [[ $char == $target ]] && echo "$char equals $target"
done
```
To truly get string equality, we have to quote the right-hand side so bash handles it literally: `[[ $char == "$target" ]]` -- another one of bash's quirks.


## Output

You don't need to `echo -n` -- the command substitution automatically removes all trailing newlines.

## Input

For truly capturing the input verbatim, use `IFS= read -r input` otherwise leading/trailing whitespace gets lost. Compare these:
```bash
echo "  foo  bar  " | {      read -r input; printf ">%s<\n" "$input"; }
echo "  foo  bar  " | { IFS= read -r input; printf ">%s<\n" "$input"; }
```
## Exercises

### atbash-cipher

If you consider the enciphering algorithm, you'll notice that the cipher array is exactly the same as the decipher array. This implies that the only difference between the encode and decode functions would be adding spaces for encode. See how you can use this to simplify your code.

<!-- -->

A further note: you can write your case statement like this, if you want:

    case "$1" in
        encode|decode) $1 "$2" ;;
        *)             exit 1 ;;
    esac

