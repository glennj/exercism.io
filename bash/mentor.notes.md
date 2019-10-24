
## Arithmetic

---

bash can do arithmetic, you don't need to call out to `bc`. See [Arithmetic Expansion](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion) in the manual.

---

You don't need to nest the arithmetic: this is OK
```bash
    # instead of this
    if (( $(( $1 % num )) == 0 )); then
    # do this
    if (( $1 % num == 0 )); then
```

---

Note that you don't need `$` for normal variables inside an arithmetic expression: more C-like arithmetic looks nicer.

## Variables

For lines 17-21, consider the `${var:-default value}` form of [Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion).

---

Get out of the habit of using ALLCAPS variable names, leave those as reservied by the shell. One day you'll write `PATH=something` and then wonder why your script is broken.

## Quoting

Unquoted variables are subject to [word splitting](https://mywiki.wooledge.org/WordSplitting)

---

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
[ -n $1 ] && echo true || echo false
```
What happens when $1 is unset or an empty string: bash will substitute the `$1` parameter with an empty string. Then bash will have this command:
```bash
[ -n  ] && echo true || echo false
```
The `[` and `[[` commands do their comparisons based on *the number of arguments given*. When there is only a single argument (excluding the mandatory closing `]` or `]]`) then the comparison is "true" if the argument is non-empty. Here there is a single argument "-n" and it is non-empty. This test incorrectly echoes "true". 

---

Note that the `==` operator in bash is not a string equality operator, it is a _pattern matching_ operator. So this may give surprising results:
```bash
target="?"
for char in {a..z}; do
    [[ $char == $target ]] && echo "$char equals $target"
done
```
To truly get string equality, we have to quote the right-hand side so bash handles it literally: `[[ $char == "$target" ]]` -- another one of bash's quirks.

---

`[[` does not do word splitting or filename expansion on unquoted variables, so I say that it has few surprises. Consider:
```
$ var=""
$ [[ -n $var ]] && echo "not empty" || echo empty
empty
$ [ -n $var ] && echo "not empty" || echo empty
not empty
```
and 
```
$ var="*"
$ [[ -n $var ]] && echo "not empty" || echo empty
not empty
$ [ -n $var ] && echo "not empty" || echo empty
bash: [: too many arguments
empty
```

---

The deep dive: the `test`, `[` and `[[` commands all act the same in that
they do different things based on *how many arguments they are given*
(excluding the closing `]`/`]]`).

* with 1 argument, the test will be successful if the argument is not empty
* with 2 arguments, the arguments are treated as a unary operator (such as
  `-n`) and an operand.
* with 3 arguments, the arguments are treated as an operand, a binary
  operator (such as `=`) and another operand
* more than 3 arguments, `test` and `[` give a "too many arguments" error,
  and `[[` may give a different error (it has a more complicated parser).

Now, looking at the `[ -n $var ]` example.

1. Since `[[` does not do **word splitting**, it knows where variable values are.
When `var=""`, given `[[ -n $var ]]`, after parameter expansion, bash will see `[[ -n
"" ]]` which is clearly **false**.

    But for `test` and `[`, word splitting is in effect. So `[ -n $var ]`
    becomes `[ -n  ]` (the empty string disappears). Now, `[` sees one
    argument *which is a non-empty string*, and is therefore **true**. 

    If you quote the variable: `[ -n "$var" ]`, then you'll get the
    expected false result.

1. Since `[[` does not do **filename expansion**, when `var="*"`, then bash
sees `[[ -n "*" ]]` which is clearly **true**.

    But for `test` and `[`, bash sees `[ -n * ]` and then expands the
    pattern to `[ -n "list" "of" "files" "in" "current" "directory" ]`.
    You'll see the "too many arguments" error (but maybe not depending on
    how many files are in your current directory). Since there is an error,
    the exit status of `[` is non-zero and the **false** branch is taken.


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

---

A further note: you can write your case statement like this, if you want:

    case "$1" in
        encode|decode) $1 "$2" ;;
        *)             exit 1 ;;
    esac

