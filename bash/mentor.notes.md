#Bash

## Shebang

The first 2 characters of the program file should be `#!`. It is recommended
that bash scripts start with
```
#!/usr/bin/env bash
``` 
That instructs the OS to run your script with bash (if you make it
executable), and it allows people reading your code to know that 
this is specifically a bash program. 

<!-- ........................................................ -->
## Backticks

Use `$(...)` instead of `` `...` `` -- see
[https://mywiki.wooledge.org/BashFAQ/082](https://mywiki.wooledge.org/BashFAQ/082)
for more details.

<!-- ........................................................ -->
## Arithmetic

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

Note that you don't need `$` for normal variables inside an arithmetic
expression ($ is still required for positional parameters and expansions
such as `${#var}`)

<!-- -->

The difference between `$((...))` and `((...))`:

`$((...))` is an arithmetic *expression* -- it returns the result of the
expression as a **value**, so you can assign it to a variable like you do in
line 10. Ref: [Arithmetic Expansion in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion)

`((...))` is the arithmetic *conditional construct*. It returns the result
of the expression as its **exit status** so you can use it in an `if`, `for`
or `while` command. See [Conditional Constructs in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down a bit.

I find the `((...))` syntax very tidy. You can assign to a variable within
it, as I demonstrated above, without checking the exit status.

One caveat, a source of subtle errors: it does not play well with [`set -e`,
the "errexit"
setting](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin).
The manual says this about `((...))`

> If the value of the expression is non-zero, the return status is 0; otherwise the return status is 1
```bash
$ bash -c '
    set -e
    count=2
    ((count -= 1))
    echo "you will see this"
    ((count -= 1))
    echo "you will not see this"
'
you will see this
```
Because the 2nd arithmetic expression had value zero, the command returned
1, and then the shell exited (with status 1) due to `set -e`.

<!-- ........................................................ -->
## Variables

For lines 17-21, consider the `${var:-default value}` form of [Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion).

<!-- -->

Get out of the habit of using ALLCAPS variable names, leave those as reserved by the shell. One day you'll write `PATH=something` and then [wonder why your script is broken](https://stackoverflow.com/q/28310594/7552).

<!-- ........................................................ -->
## Quoting

Unquoted variables are subject to [word splitting](https://mywiki.wooledge.org/WordSplitting)

<!-- ........................................................ -->
## Assignment

You can use the `+=` concatenating assignment operator: these are equivalent:
```bash
foo=${foo}bar
foo+=bar
```

<!-- ........................................................ -->
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

<!-- ........................................................ -->
## Output

You don't need to `echo -n` -- the command substitution automatically removes all trailing newlines.

<!-- ........................................................ -->
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

<!-- ........................................................ -->
# Exercises

## atbash-cipher

If you consider the enciphering algorithm, you'll notice that the cipher array is exactly the same as the decipher array. This implies that the only difference between the encode and decode functions would be adding spaces for encode. See how you can use this to simplify your code.

<!-- -->

A further note: you can write your case statement like this, if you want:

    case "$1" in
        encode|decode) $1 "$2" ;;
        *)             exit 1 ;;
    esac

<!-- ........................................................ -->
## two-fer

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

<!-- ........................................................ -->
## raindrops

There is a more concise way to handle the maybe-empty variable: I suggest
looking into the `${var:-default}` form of parameter expansion [here in the
manual](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html).

<!-- -->

You can use the `+=` concatenating assignment operator: these are equivalent:
```bash
foo=${foo}bar
foo+=bar
```

<!-- -->

Instead of putting an arithmetic expansion inside the string-oriented `[[...]]`, 
you can use the arithmetic conditional construct (see [here in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down to `((...))`):
```bash
    # instead of this
    if [[ $(( $1 % num )) == 0 ]]; then
    # do this
    if (( $1 % num == 0 )); then
```

<!-- ........................................................ -->
## markdown

You _can_ do this with just plain bash. The regular expression support is
very useful. For example, to handle the headers, instead of calling `expr`,
you could
```bash
if [[ $line =~ ^("#"+)" "+(.*) ]]; then
    tag="h${#BASH_REMATCH[1]}"
    html+="<$tag>${BASH_REMATCH[2]}</$tag>"
```

Dealing with strong text is particularly nasty. In fact, the way you've
implemented it breaks if there is an underscore inside the double underscore
delimiters (note, I'm using `sed -E` for extended regexes, and `s!!!` instead
of `s///` to avoid escaping the close tag)
``` 
$ echo "one __two three__ four_five __six_seven__ eight" | sed -E 's!__([^_]+)__!<strong>\1</strong>!g'
one <strong>two three</strong> four_five __six_seven__ eight
```
And, as I'm sure you discovered working on the problem you can't just accept
any content between the delimiters because it's too greedy
```
$ echo "one __two three__ four_five __six_seven__ eight" | sed -E 's!__(.+)__!<strong>\1</strong>!g'
one <strong>two three__ four_five __six_seven</strong> eight
```
And if you think, OK I'll capture stuff *before* the first delimiter, then
that still breaks since it *consumes* the leading text:
```
$ echo "one __two three__ four_five __six_seven__ eight" | sed -E 's!(.*)__(.+)__!\1<strong>\2</strong>!g'
one __two three__ four_five <strong>six_seven</strong> eight
```
With sed, you have to do it iteratively in a loop. The `:` command
introduces a "label" that you can jump to, and the `t` command conditionally jumps to that
label if a substitution has been made since the last `t`:
```
$ echo "one __two three__ four_five __six_seven__ eight" | sed -E ':a; s!(.*)__(.+)__!\1<strong>\2</strong>!; ta'
one <strong>two three</strong> four_five <strong>six_seven</strong> eight
```
Because the *first* `.*` is greedy, this will repeatedly match the *last* pair
of delimiters, and do the replacement, until no more are found.

Perl is easier to use here due to its non-greedy regex quantifiers:
```
$ echo "one __two three__ four_five __six_seven__ eight" | perl -pe 's{__(.+?)__}{<strong>$1</strong>}g'
one <strong>two three</strong> four_five <strong>six_seven</strong> eight
```

However, this is do-able in bash, using the same strategy as sed: loop  and replace until no more
matches are found: this concise snippet from IsaacG's solution:
```bash
while [[ $line =~ ^(.*)__(.+)__(.*) ]]; do
    printf -v line "%s<strong>%s</strong>%s" "${BASH_REMATCH[@]:1:3}"
done
```
Phew!

<!-- ........................................................ -->
# Exercism/Philosophy

Well, it depends on the purpose of the scripts you write, I suppose. If it
is going to run on multiple machines, then portability will be a concern, so
you can't target the latest and greatest bash features. If your scripts are
going to have to run on multiple OS's, then you have to worry about how
portable your external code is: sed on MacOS is different from GNU sed is
different from AIX and Solaris ... In my work experience, bash scripts I
write live in one place on the server, so portability really hasn't been a
concern.

I currently work on a Mac, which ships with the very old bash 3.2. But with
Homebrew it's super easy to install bash 5.

Mainly, here on exercism, I consider the bash track to be where you can
learn about bash-specific features.

