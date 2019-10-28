
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

<!-- -->

The difference between `$((...))` and `((...))`:

`$((...))` is an arithmetic *expression* -- it returns the result of the expression as a **value**, so you can assign it to a variable like you do in line 10. Ref: [Arithmetic Expansion in the manual](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion)

`((...))` is the arithmetic *conditional construct*. It returns the result of the expression as its **exit status** so you can use it in an `if`, `for` or `while` command. See [Conditional Constructs in the manual](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs) and scroll down a bit.

I find the `((...))` syntax very tidy. You can assign to a variable within it, as I demonstrated above, without checking the exit status.

One caveat, source of subtle errors: it does not play well with [`set -e`, the "errexit" setting](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin). The manual says:

> If the value of the expression is non-zero, the return status is 0; otherwise the return status is 1
```bash
$ bash -c 'set -e; count=1; ((count--)); echo "you will see this"; ((count--)); echo "you will not see this"'
you will see this
```

Because the 2nd arithmetic expression had value zero, the command returned 1, and then the shell exited due to set -e.

## Variables

For lines 17-21, consider the `${var:-default value}` form of [Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion).

<!-- -->


Get out of the habit of using ALLCAPS variable names, leave those as reserved by the shell. One day you'll write `PATH=something` and then [wonder why your script is broken](https://stackoverflow.com/q/28310594/7552).

## Quoting

Unquoted variables are subject to [word splitting](https://mywiki.wooledge.org/WordSplitting)

<!-- -->

## Assignment

You can use the `+=` concatenating assignment operator: these are equivalent:
```bash
foo=${foo}bar
foo+=bar
```

## Conditionals

In bash, prefer `[[...]]` over `[...]`. The double bracket conditional command gives you more features (including regular expression matching), and fewer surprises (particularly about unquoted variables).

<!-- -->

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
I can go into greater detail about why `[` gives incorrect results if you want.


<!-- -->

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

<!-- -->

Note that the `==` operator in bash is not a string equality operator, it is a _pattern matching_ operator (see [here in the manual](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b)). So this may give surprising results:
```bash
target="?"
for char in {a..z}; do
    [[ $char == $target ]] && echo "$char equals $target"
done
```
To truly get string equality, we have to quote the right-hand side to force bash to take it literally: `[[ $char == "$target" ]]` 

Another one of bash's quirks.

<!-- -->

I tend to avoid using regular expressions unless I have a matching problem more complicated than what I can achieve with [glob patterns](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching), or I need to [capture sub-patterns](https://www.gnu.org/software/bash/manual/bash.html#index-BASH_005fREMATCH).

## Output

You don't need to `echo -n` -- the command substitution automatically removes all trailing newlines.

## Input

For truly capturing the input verbatim, use `IFS= read -r input`

* `IFS=` preserves leading/trailing whitespace, and 
* `-r` is necessary to maintain backslashes as they appear in the data.

Compare these:
```bash
echo "  foo  \t  bar  " | {      read    input; printf ">%s<\n" "$input"; }
echo "  foo  \t  bar  " | {      read -r input; printf ">%s<\n" "$input"; }
echo "  foo  \t  bar  " | { IFS= read -r input; printf ">%s<\n" "$input"; }
```

<!--
    Although, using the default REPLY variable grabs the data with
    whitespace:
        $ echo "  foo  \t  bar  " | { read -r ; printf ">%s<\n" "$REPLY"; }
        >  foo  \t  bar  <
-->

## Exercises

### atbash-cipher

If you consider the enciphering algorithm, you'll notice that the cipher array is exactly the same as the decipher array. This implies that the only difference between the encode and decode functions would be adding spaces for encode. See how you can use this to simplify your code.

<!-- -->

A further note: you can write your case statement like this, if you want:

    case "$1" in
        encode|decode) $1 "$2" ;;
        *)             exit 1 ;;
    esac

### two-fer

There is a more concise way to manage the optional input here. I suggest
looking into the `${var:-default}` form of parameter expansion [here in the
manual](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html).

<!-- -->

In bash, prefer `[[...]]` over `[...]`. It's more powerful and less likely to act
in unexpected ways.

<!-- -->

Generally, you should encapsulate the main body of your script in a `main`
function, like you may remember from the Hello World exercise.  It
encapsulates a chunk of logic in one function to encourage re-use.
It is good practice, and becomes more and more important as your programs get bigger.

