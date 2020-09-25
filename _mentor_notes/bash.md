# Bash

TOC
* [How bash interprets command](#how-bash-interprets-command)<br>
* [Shebang](#shebang)<br>
* [Backticks](#backticks)<br>
* [Arithmetic](#arithmetic)<br>
* [Parameter Expansion](#parameter-expansion)<br>
    * [Special parameters](#special-parameters)<br>
* [Quoting](#quoting)<br>
* [Assignment](#assignment)<br>
* [Conditionals](#conditionals)<br>
* [Loops](#loops)<br>
* [Output](#output)<br>
* [Input](#input)<br>
* [Boolean Operators](#boolean-operators)<br>
* [Functions](#functions)<br>
    * [Passing an array to a function](#passing-an-array-to-a-function)<br>
* [Very rare and subtle mistakes](#very-rare-and-subtle-mistakes)<br>
* Exercism
    * [Exercism/Philosophy](#exercismphilosophy)<br>
    * [Exercism/Testing](#exercismtesting)<br>
* [Miscellaneous notes to be organized](#miscellaneous-notes-to-be-organized)<br>

Exercises

* [two-fer](#two-fer)
* [raindrops](#raindrops)
* [atbash-cipher](#atbash-cipher)
* [markdown](#markdown)
* [bob](#bob)
* [hamming](#hamming)
* [tournament](#tournament)
* [acronym](#acronym)
* [grep](#grep)


Be sure to check out [the community
solutions](https://exercism.io/tracks/bash/exercises/${SLUG}/solutions) to
see other approaches.

<!-- ============================================================ -->

## How bash interprets a command

A bash command is a "metacharacter"-separated list of words and operators;
the first word is the command (except for optional leading `var=value`
words).

One the key things to learn to really grok bash is [3.1.1 Shell
Operation](https://www.gnu.org/software/bash/manual/bash.html#Shell-Operation),
and particularly [3.5 Shell
Expansions](https://www.gnu.org/software/bash/manual/bash.html#Shell-Expansions): 
* bash gets a command to execute
* it is split into words and operators
* the words are subject to expansions
* redirections are applied
* and finally the command is executed

---

## Testing

Make use of the test suite provided for you. See [Running the
Tests](https://exercism.io/tracks/bash/tests).

<!-- ........................................................ -->
## Shebang

It is recommended that bash scripts start with
```bash
#!/usr/bin/env bash
``` 
That instructs the OS to run your script with bash (if you make it
executable), and it allows people reading your code to know that 
this is specifically a bash program. 

<!-- -->

We usually recommend you use 
```bash
#!/usr/bin/env bash
```
That makes the script a bit more portable (for systems where bash is not located in /bin)

It does expose you to possible version conflicts (for example if you use a
bash 4.3 feature in your code, but the bash found in the path is an older
version)

<!-- ........................................................ -->
## Backticks

Use `$(...)` instead of `` `...` `` -- see
[https://mywiki.wooledge.org/BashFAQ/082](https://mywiki.wooledge.org/BashFAQ/082)
for more details.

<!-- ........................................................ -->
## Functions

It's a good habit to use `local` for function variables. Then the global
namespace is not polluted with every variable.

<!-- -->

Note that `local` variables show up in the scope of functions *called
from where the variable was declared*. So you can do this:
```bash
foo() {
	local x=$1
	bar
}

bar() {
	echo "parameter is $x"
}

x=global
echo $x
foo 42
echo $x
```

Sometimes this helps readability, sometimes not, so when it makes sense
pass variables as function arguments.

<!-- -->

A good design practice is to make functions as single-purpose as possible.
If you have a function that does, say, a bunch of validation and then some
calculations, you might consider breaking the function up:
```
main() {
	validate "$@"
	hamming "$@"
}

validate() { 
	do checks and exit if errors
}

hamming() { 
	count=...
	echo $count
}

main "$@"
```

### Passing an array to a function

Regarding passing arrays around, there are 3 ways to do it without stringifying:

1. **pass by value**: send all the array values as arguments to a function, and
    the function can reassemble the values into a local array. This makes it
    impossible to update an array in-place.
    ```bash
    function byval() {
        local ary=( "$@" )
        declare -p ary
    }
    my_array=( a b "c d" e )
    byval "${my_array[@]}"
    ```
    Double quoting is absolutely crucial here to ensure array elements
    containing whitespace are maintained as units.

1. **pass by name**: send the array **name** to the function and use indirect
    variables to slurp the values into the function. Again, no in-place
    modifications. The syntax here is gross.
    ```bash
    function byname() {
        local ary_name=$1
        local tmp="${ary_name}[@]"  # this is a plain string
        local ary=( "${!tmp}" )
        declare -p ary
    }
    byname my_array
    ```

1. **pass by reference**: this requires bash version 4.3+. Send the array
    name to the function, but the function establishes it as a reference to
    the caller's variable. This allows the array to be updated.
    ```bash
    function byref() {
        local -n ary=$1
        declare -p ary
        ary[2]="Hello, World!"
    }
    byref my_array
    declare -p my_array
    ```


<!-- ........................................................ -->
## Arithmetic

bash can do arithmetic, you don't need to call out to `bc`. See [Arithmetic
Expansion](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion)
in the manual.

<!-- -->

You can omit the `$` for variables inside an arithmetic expression.
See [this Shellcheck wiki entry](https://github.com/koalaman/shellcheck/wiki/SC2004) for details.

<!-- -->

<details><summary>You can assign to variables within an arithmetic
expression (click for details):</summary><p> 
Instead of
```bash
total=$(( total + increment ))
```
You can write
```bash
(( total += increment ))
```
</p></details>

<!-- -->

`((...))` is preferred over `let`. See [the let builtin
command](https://wiki.bash-hackers.org/commands/builtin/let) for details.

<!-- -->

You don't need to nest the arithmetic:
```bash
    # instead of this
    if (( $(( a % b )) == 0 )); then
    # do this
    if (( a % b == 0 )); then
```

<!-- -->

Within an arithmetic expression, bash allows for variable names without the `$`, so:
```bash
if (( number % 4 == 0 ))
```
This allows for readable C-like expressions.

It works for array elements too:
```bash
ary=(41 42 43)
echo $(( ary[1] * 2 )) # => 84
```

The index part of numerically indexed arrays is an arithmetic expression:
```bash
fibonacci=(1 1)
for ((i=2; i<=10; i++)); do
    (( fibonacci[i] = fibonacci[i-2] + fibonacci[i-1] ))
done
declare -p fibonacci 
```

As well as the offset and length parts of the `${var:offset:length}` parameter expansion:
```bash
str='Hello world'
i=4
echo "${str:i:1},${str:i + 2:6 - 4}" # => "o,wo"
```

<!-- -->

The difference between `$((...))` and `((...))`:

`$((...))` is an arithmetic *expression* -- it returns the result of the
expression as a **value**, so you can assign it to a variable like `answer=$(( 6 * 9 ))`. 
Ref: [Arithmetic Expansion in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion)

`((...))` is the arithmetic *conditional construct*. It returns the result
of the expression as its **exit status** so you can use it in an `if`, `for`
or `while` command. See [Conditional Constructs in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down a bit.

I find the `((...))` syntax very tidy. You can assign/update a variable within
it without checking the exit status:
```bash
(( result += $a + $b ))
(( count++ ))
```

One caveat, a source of subtle errors: it does not play well with [`set -e`,
the "errexit"
setting](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin).
The manual says this about `((...))`

> If the value of the expression is non-zero, the return status is 0;
> otherwise the return status is 1
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


<!-- -->

For check a value is in a range, instead of
```bash
[[ "$arg" -gt 0 && "$arg" -le 64 ]]
```
you can use the arithmetic
```bash
(( 1 <= arg && arg <= 64 ))
```
That's as close to the `1 ≤ arg ≤ 64` mathematical notation as you'll get

<!-- ........................................................ -->
## Parameter Expansion

Consider the `${var:-default value}` form of [Shell Parameter
Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion).

<!-- -->

Get out of the habit of using ALLCAPS variable names, leave those as
reserved by the shell. One day you'll write `PATH=something` and then
[wonder why](https://stackoverflow.com/q/27555060/7552) 
[your script is broken](https://stackoverflow.com/q/28310594/7552).

If you're using ALLCAPS to indicate a constant, use the `readonly` or
`declare -r` builtins to let the shell know too.

<!-- -->

In the `${var:index:length}` form of parameter expansion, both the `index`
and `length` parts are arithmetic expressions: you don't need to use `$` for
variables in an arithmetic expression. So you can write:
```bash
char="${string:i:1}"
# .............^
```

<!-- ........................................................ -->
### Special parameters

Use `set` with no options to assign to the positional parameters $1, $2, etc
```bash
set -- "a  b" "c * d" e
```
Now, $1 equals "<code>a &nbsp;b</code>" (with 2 spaces); $2 equals "`c * d`" and $3 equals "`e`".

These two special parameters are extremely useful:
* `"$*"` -> all the arguments are concatenated into a single string.  
* `"$@"` -> expands so that each argument is separate.

When they are unquoted, the usual shell 
[Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting)
and [Filename Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion)
is allowed to happen.

It's best illustrated with an example.
Given a little testing function (note: `printf` will reuse the format string
as many times as necessary to consume all its arguments):
```bash
check_args() {
  echo "I have $# arguments:"
  printf ">%s<\n" "$@"
}
```
We can demonstrate:  
* into 1 string
    ```bash
    $ check_args "$*"
    I have 1 arguments:
    >a  b c * d e<
    ```
* each parameter
    ```bash
    $ check_args "$@"
    I have 3 arguments:
    >a  b<
    >c * d<
    >e<
    ```
* word splitting and filename expansion allowed (unquoted `$*` will behave exactly the same):
    ```bash
    $ check_args $@
    I have 8 arguments:
    >a<
    >b<
    >c<
    >README.md<
    >bob.sh<
    >bob_test.sh<
    >d<
    >e<
    ```
`"$*"` uses the first character of `$IFS` as the join character. By default,
that is a space. If we redefine IFS, we can see the effect:
```bash
$ IFS=":"
$ check_args "$*"
I have 1 arguments:
>a  b:c * d:e<
```

<!-- ........................................................ -->
## Quoting

Unquoted variables are subject to [word
splitting](https://mywiki.wooledge.org/WordSplitting) and
[glob](https://mywiki.wooledge.org/glob) expansion.

<!-- -->

Ah, that's the magic of quoting: given a script invocation `script.sh "a b" "c d"`:

* with `main "$@"`, main receives 2 parameters
* with `main $@`, main receives 4 parameters
* with `main $*`, main receives 4 parameters
* with `main "$*"`, main receives 1 parameter

<!-- this specific comment on a ReverseString solution -->

The problem is unquoted variables.

When bash is processing a line from your script and figuring out how to
execute it, the line gets split into tokens (using whitespace) and the
tokens are subject to a list of 
[Shell Expansions](https://www.gnu.org/software/bash/manual/bash.html#Shell-Expansions).
Two of the expansions are 
[Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting)
and [Filename Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion)
-- these 2 expansions don't happen within quotes.

What's happening with that test is:

* after the loop, the $reverseString variable holds the value `"b  * a "`
* then you echo the variable **without quotes**
* the shell turns `echo ${reverseString}` into `echo b  * a` 
* then word splitting happens, and glob expansion turns `*` into the list of
  files in the current directory.

Run the test manually with tracing turned on and you'll see what's going on:
```bash
bash -x reverse_string.sh " a *  b"
```

There's a long writeup about it here: [Security implications of forgetting
to quote a variable in bash/POSIX shells](https://unix.stackexchange.com/q/171346/4667)

<!-- -->

([responding to a question](https://exercism.io/mentor/solutions/9206be1023cb490a8d0a0d93d8cf15b4?iteration_idx=6#discussion-post-669985))

Always quote your variables, unless you know exactly when (and why!) to
leave them unquoted. Sometimes you _want_ word splitting or pathname
expansion, but most often you don't.

Sometimes though, you know exactly what your variables will contain. For example 

* on line 26, you know that's a number, and you have not altered IFS so you
  know it's safe to leave unquoted.
* or line 18, the variable of a case statement is documented to be exempt
  from word splitting and pathname expansion (but it's no harm to quote that
  variable).
* like line 15, it's documented that variables inside `[[...]]` are not
  subject to word splitting and pathname expansion (but it's no harm to
  quote them).
* however, since `==` is a _pattern matching_ operator inside `[[...]]`, if
  you want to do _equality_ comparison and the right-hand side of == is a
  variable, you have to quote that variable so any special glob characters
  are treated as plain characters.
    ```bash
    y="?"
    [[ $x == $y ]] && echo "x is any one character"
    [[ $x == "$y" ]] && echo "x is a question mark"
    ```

Like many things in bash, it's complicated, and there are exceptions to just about everything. 

<!-- ........................................................ -->
## Assignment

<details><summary>You can use the `+=` concatenating assignment operator:
click for details</summary><p>
These are equivalent:
```bash
foo="${foo}bar"
foo+="bar"
```
Full details at [Shell
Parameters](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters)
in the manual.
</p></details>

<!-- ........................................................ -->
## Loops

<details><summary>It's not necessary to call out to <code>seq</code>: use
bash's builtin C-style for loop (click for details):</summary><p>

```bash
len=${#input}
for (( i = 0; i < len; i++ )); do ...
```

See [Looping
Constructs](https://www.gnu.org/software/bash/manual/bash.html#Looping-Constructs)
in the manual.
</p></details>

<!-- ........................................................ -->
## Conditionals

Within `[[...]]`, the `=`,`<`,`>`,... operators do **_string_**
comparison. For _numeric_ comparisons use `-eq`, `-lt`, `-gt`, ... operators
respectively. At a bash prompt, type `help test` for details.

<!-- -->

In bash, prefer `[[...]]` over `[...]`. The double bracket [conditional
command](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b)
gives you more features (including regular expression matching), and fewer
surprises (particularly about unquoted variables).

<!-- Some more details -->

`[[` does not do word splitting or filename expansion on unquoted variables,
so I say that it has few surprises. Consider:
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


<!-- Way more details -->

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

Note that the `==` operator in bash is not a string equality operator, it is
a _pattern matching_ operator (see [here in the
manual](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b)).
So this may give surprising results:
```bash
target="?"
for char in {a..z}; do
    [[ $char == $target ]] && echo "$char equals $target"
done
```
To truly get string equality, we have to quote the right-hand side to force
bash to take it literally: `[[ $char == "$target" ]]` 

Another one of bash's quirks.

<!-- -->

I tend to avoid using regular expressions unless I have a matching problem
more complicated than what I can achieve with [glob
patterns](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching),
or I need to [capture
sub-patterns](https://www.gnu.org/software/bash/manual/bash.html#index-BASH_005fREMATCH).

<!-- -->
---

`$((` vs `((`
* the first *outputs* the result of the expression. It's suitable for things
  like `result=$((...))` or `echo $((...))`
* the second evaluates the expression and sets an *exit status*: if the
  result is zero, status is 1; otherwise status is zero. It is this that
  makes `((...))` suitable as the condition of an `if/for/while` statement.

Keep in mind that bash arithmetic expressions allow for assignment. These
two are equivalent, but one is more readable:
```bash
result=$((result + 1))
(( result += 1 ))
```
---
One thing to note about `((...))` and `set -e`: the "errexit" setting aborts
the script if any command exits with a non-zero status. 
```bash
$ bash -c '
    set -e
    count=2
    ((--count))
    echo "you will see this"
    ((--count))
    echo "you will not see this"
'
```
This is just one of the reasons I don't use `set -e`: 
See [http://mywiki.wooledge.org/BashFAQ/105](http://mywiki.wooledge.org/BashFAQ/105)

<!-- -->
---

`[[` vs `((`

First, how does `if` work?
```bash
$ help if
if: if COMMANDS; then COMMANDS; [ elif COMMANDS; then COMMANDS; ]... [ else COMMANDS; ] fi
    Execute commands based on conditional.
```
Note there's nothing in there about `[[` or `((`. The conditional is the
exit status of COMMANDS. This is a common usage:
```bash
if grep -q pattern file; then
    echo "file contains pattern"
fi
```
The COMMAND can also be a shell function, so this can lead to very tidy
code.

So `[[` or `((`? As you've seen, `((` is only for arithmetic expressions.
`[[` is for anything else.

To see what `[[` can do, do this at a shell prompt:
```
$ help [ test | less
```
A couple of specific things to point out:

* `[[ $a == $b ]]` -- the `==` and `!=` operators are not just for string
  equality, they are _pattern matching_ operators:
    ```bash
    [[ $string == *foo* ]] && echo "string contains foo"
    ```
    But the right-hand side can be quoted to remove any special meaning of
    pattern characters
    ```bash
    var="*foo*"
    [[ $string == "$foo" ]] && echo "string is exactly '*foo*'"
    ```
* `[[` has a regular expression matching operator: `=~`
    * there is no `!~` operator, so
        ```bash
        if [[ ! $string =~ $regex ]]; then
            echo "string did not match"
        fi
        ```
    * captured bits go into the `BASH_REMATCH` array:
        ```bash
        [[ "hello world" =~ (.*)(o.*o)(.*) ]]
        declare -p BASH_REMATCH
        ```
* `-v` can be useful to test if an associative array contains a key:
    ```bash
    declare -A map=([foo]=bar [baz]=qux)
    key="blah"
    if [[ -v map[$key] ]]; then
        echo "$key is in the map"
    fi
    ```

`[[` vs `[`: In bash, prefer `[[...]]` over `[...]`. The double bracket
conditional command gives you more features, and fewer surprises
(particularly about unquoted variables).



<!-- ........................................................ -->
## Boolean Operators

You have to be a bit careful with `A && B || C` -- C will execute if
**either** A **or** B fails.

With `if A; then B; else C; fi` the only time C runs is if A fails,
regardless of what happens with B.

<!-- ........................................................ -->
## Output

You don't need to `echo -n` -- the command substitution automatically
removes all trailing newlines.

<!-- -->

It is very important to quote your variables. Try this:
```bash
var="  *"
echo $var
# then
echo "$var"
```
There are even [Security implications of forgetting to quote a variable in
bash/POSIX shells](https://unix.stackexchange.com/q/171346/4667)

<!-- -->

Always use a format specifier for `printf` -- if the string to be printed
contains a `%` character, you'll get an error. See [this shellcheck
explanation](https://www.shellcheck.net/wiki/SC2059)

<!-- -->

There's a "useless" echo here: `echo $(cmd ...)` when `cmd` already prints
to stdout, it's merely extra overhead to capture that output only to print
it to stdout. More details at
[the shellcheck wiki](https://github.com/koalaman/shellcheck/wiki/SC2005)

<!-- ........................................................ -->
## Input

For truly capturing the input verbatim, use `IFS= read -r input`
* `IFS=` preserves leading/trailing whitespace, and 
* `-r` is necessary to maintain backslashes as they appear in the data.

Compare these:
```bash
echo "  foo  \t  bar  " | {      read    input; printf '"%s"\n' "$input"; }
echo "  foo  \t  bar  " | {      read -r input; printf '"%s"\n' "$input"; }
echo "  foo  \t  bar  " | { IFS= read -r input; printf '"%s"\n' "$input"; }
```

<!--
    Note to self:
    Although, using the default REPLY variable grabs the whole line with whitespace:
        $ echo "  foo  \t  bar  " | { read -r ; printf '"%s"\n' "$REPLY"; }
        "  foo  \t  bar  "
-->

<!-- ........................................................ -->
# Very rare and subtle mistakes

## reading a file and the last line of the file does not end with a newline

```bash
{ echo "foo"; echo -n "bar"; } | while IFS= read -r line; do echo "$line"; done
```
outputs only "foo". 

What's happening here? `IFS= read -r line` reads the characters "bar" into
the variable but then exits with a non-zero status, due to the missing
newline. The while loop ends.

The absolutely safe way to read a line from a file, even if the last line is
missing a newline is:
```bash
while IFS= read -r line || [[ -n $line ]]; do ...
```
This loops while `read` reads a whole newline-terminated line OR `read`
reads some characters.

<!-- -->

To accurately read the lines of a file, use a `while read` loop: see [bash
FAQ #1](http://mywiki.wooledge.org/BashFAQ/001).

<!-- ........................................................ -->
# Exercism/Philosophy

This is a fine *shell* solution. I challenge you to use bash builtin
features instead of external tools.

<!-- -->

_(Responding to a comment about using bash features vs external tools)_

Well, it depends on the purpose of the scripts you write, I a. If it
is going to run on multiple machines, then portability will be a concern, so
you can't target the latest and greatest bash features. If your scripts are
going to have to run on multiple OS's, then you have to worry about how
portable your external code is: sed on MacOS is different from GNU sed is
different from AIX and Solaris ... In my work experience, bash scripts I
write live in one place on one server, so portability really hasn't been a
concern.

I currently work on a Mac, which ships with the very old bash 3.2. But with
Homebrew it's super easy to install bash 5.

Mainly, here on exercism, I consider the bash track to be where you can
learn about bash-specific features.

<!-- -->

Why not use `set -e` (`set -o errexit`)?
See [http://mywiki.wooledge.org/BashFAQ/105](http://mywiki.wooledge.org/BashFAQ/105)

<!-- ........................................................ -->
# Exercises

## two-fer

In bash, prefer `[[...]]` over `[...]`. It's more powerful and less likely to act
in unexpected ways.

<!-- -->

There is a more concise way to manage the optional input here. I suggest
looking into the `${var:-default}` form of parameter expansion [here in the
manual](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html).

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
You may have seen this already in the "two-fer" exercise.

<!-- -->

<details><summary>You can use the <code>+=</code> concatenating assignment
operator: click for details</summary>
These are equivalent:
```bash
foo="${foo}bar"
foo+="bar"
```
</details>

<!-- -->

<details><summary>Instead of putting an arithmetic expansion inside the
string-oriented <code>[[...]]</code>, you can use the arithmetic conditional
construct. Click for details.</summary>

```bash
# instead of this
if [[ $(( $1 % num )) == 0 ]]; then
# do this
if (( $1 % num == 0 )); then
```
see [here in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down to `((...))`
</details>

<!-- -->

Instead of looping over all the numbers from 1 to _num_, you only
need to test the remainder of 3, 5 and 7.

<!-- ........................................................ -->
## atbash-cipher

If you consider the enciphering algorithm, you'll notice that the cipher
array is exactly the same as the decipher array. This implies that the only
difference between the encode and decode functions would be adding spaces
for encode. See how you can use this to simplify your code.

<!-- -->

A further note: you can write your case statement like this, if you want:

    case "$1" in
        encode|decode) $1 "$2" ;;
        *)             exit 1 ;;
    esac

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
## bob

You may find this simpler to take a step back and look at the problem abstractly:
```bash
if input is silence
    echo "Fine. Be that way!"
else if input is yelling and question
    echo "Calm down, I know what I'm doing!"
else if input is just yelling
    echo 'Whoa, chill out!'
else if input is just a question
    echo 'Sure.'
else any other input
    echo 'Whatever.'
```

<!-- -->

<details><summary>Refactor the conditions into functions. (expand for spoilers)</summary>

```bash
shouting() {
  [[ $1 =~ [[:upper:]] ]] && [[ ! $1 =~ [[:lower:]] ]]
}
asking() {
  [[ $1 =~ [?][[:space:]]*$ ]]
}
silent() {
  [[ $1 =~ [^[:space:]] ]]
}

if silent "$1"; then ...
elif asking "$1"; then
  if shouting "$1"; then ...
  else ...
  fi
elif shouting "$1"; then ...
else ...
fi
```
This works because `if` takes a _command_ as its first argument, and `[[` is a
command (it's actually a "keyword" but acts like a command). So any command
is valid, and the success/failure is based on the _exit status_ of the
command. See `help if` at a bash prompt.

</details>
<details><summary>Another method is to perform each test in advance and
capture the exit status</summary>

```bash
[[ $1 =~ [A-Z] ]] && [[ ! $1 =~ [a-z] ]]; shouting=$?
...
if [[ $shouting -eq 0 ]]; ...
```
or convert the 0/1 exit status into a 1/0 boolean
```bash
[[ $1 =~ [A-Z] ]] && [[ ! $1 =~ [a-z] ]]; shouting=$(( ! $? ))
...
if (( shouting )); ...
```
</details>

<!-- -->

Notice that yelling and question appear twice? That indicates we should try
to put that in some reusable form, like a variable or a function.

Let's look at these factors and see how to satisfy them:
* silence is easy, $input is empty
* question is simple too, $input ends with a question mark
* yelling can be described as: $input contains upper case letters but no
  lower case letters. We don't care about the presence or absence of digits.

Before continuing, note that the `==` operator in bash is not a string
equality operator, it is a _pattern matching_ operator (see [here in the
manual](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b)).
So we can use `==` for our "contains an upper" test. Yelling can be tested
thusly:
```bash
[[ $input == *[[:upper:]]* ]] && [[ $input != *[[:lower:]]* ]]
```
[Bash
patterns](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching)
allow us to do a lot without having to use regular expressions.

About that "reusable form" remark. How to achieve that?

First, note the syntax of `if` (bash has a really useful interactive `help` system)
```bash
$ help if
if: if COMMANDS; then COMMANDS; [ elif COMMANDS; then COMMANDS; ]... [ else COMMANDS; ] fi
    Execute commands based on conditional.

    The `if COMMANDS' list is executed.  If its exit status is zero, then the ...
```
See that after the `if` keyword, bash wants to see COMMANDS. There's nothing
specific about `[` or `[[` or `((` -- those are just builtin commands. We
could put any any list/pipeline of commands in there, and `if` branches
based on the exit status.

I mentioned variables. Here's a technique I like to use that some people
don't like. `true` and `false` are bash builtin commands that return the
expected exit status. So you can do this:
```bash
[[ $input == *[[:upper:]]* ]] && [[ $input != *[[:lower:]]* ]] && yelling=true || yelling=false
# similar for setting `silence` and `question` variables

# `if` now uses the *values* of the variables as "COMMANDS"
if $silence; then
  echo "silence response"
elif $yelling && $question; then
  echo "shouted question response"
elif $yelling; then
  echo "yelling response"
elif $question; then
  echo "question response"
fi
```

We could also use functions to store the conditions for reuse:
```bash
yelling() {
  [[ $input == *[[:upper:]]* ]] && [[ $input != *[[:lower:]]* ]]
}
# similar for defining `silence` and `question` functions

# `if` now uses the functions as "COMMANDS"
if silence; then
  echo "silence response"
elif yelling && question; then
  echo "shouted question response"
elif yelling; then
  echo "yelling response"
elif question; then
  echo "question response"
fi
```

Note that `local` variables do show up in the scope of functions *called
from where the variable was declared*, so `$input` in `yelling` is OK. 
We could have written the following to keep the variables even more "local":
```bash
yelling() {
  [[ $1 == *[[:upper:]]* ]] && [[ $1 != *[[:lower:]]* ]]
}

# and then
# ...
elif yelling "$input" && question "$input"; then ...
```
 
<!-- ........................................................ -->
## tournament

OK, there's a few things going on here. In order from least crucial to most:

* there's not a test case for when the points are 2 digits, but I think your
  printf format should be
    ```bash
    fmt="%-30s | %2s | %2s | %2s | %2s | %2s\n"
    ```
* you need to sort by *points* not *wins*
* if the points are equal, sort by name: `sort -t, -k2,2nr -k1,1`
* you're not reading the data correctly.

    This is a tricky one, where you have to either read from stdin or from a
    named file.  The way to do this is with the `-t` test:
    ```bash
    $ help test
    test: test [expr]
    ...
          -t FD          True if FD is opened on a terminal.
    ```

    If `[[ -t 0 ]]` is true, then stdin is connected to the terminal -- i.e.
    **not redirected** --> read from the file named in $1.  
    If `[[ -t 0 ]]` is false, then stdin is a redirection --> read from stdin.

<!-- -->

Option 1: slurp the data all at once
```bash
if [[ ! -t 0 ]]; then
	# read from stdin
	data=$(cat)
else
    [[ -r $1 ]] || die "cannot read: $1"
	data=$(< "$1") 
fi

while IFS=';' read -r home away outcome; do
    # ...
done <<<"$data"
```

Option 2: if it's a file, redirect it onto stdin, and read line by line
```bash
if [[ -t 0 ]]; then
    [[ -r $1 ]] || die "cannot read: $1"
	exec 0< "$1"
	# now the script's stdin come from that file
fi

while IFS=';' read -r home away outcome; do
    # ...
done    # no redirection, read from stdin
```

<!-- ........................................................ -->
## hamming

Don't check specifically that a char is `?` -- the same problem will occur with the `*` character.

The issue is that the `==` operator (and `!=` too) is not just string
equality, it is a *pattern matching* operator. For example, to check if a
string contains a digit, you can write:
```bash
if [[ $string == *[0-9]* ]]; then echo "contains a digit"; fi
```

Patterns can be stored in variables:
```bash
has_digit='*[0-9]*'
if [[ $string == $has_digit ]]; then echo "contains a digit"; fi
```

If any parts of the pattern are quoted, then that part is taken literally:
```bash
if [[ $string == *"[0-9]"* ]]; then 
	echo "contains the exact string '[0-9]'"
fi
```

Same holds for patterns stored in variables
```bash
has_digit='*[0-9]*'
if [[ $string == "$has_digit" ]]; then
	echo "\$string is the exact string '*[0-9]*'"
fi
```

This is the concept the last test is trying to illustrate. Instead of
specifically testing the various glob metacharacters -- which is at best fragile --
you can simply ensure the right-hand operand is quoted:
```bash
		if [[ ${s:$i:1} != "${t:$i:1}" ]]; then
```

<!-- When use user does `[ ${s:i:1} != ${t:i:1} ]` in single brackets -->

Note that, since you're using the single bracket conditional on line 23,
you're exposing a slightly different bug:
```bash
$ bash hamming.sh 'AAA' 'A?A'
1
$ touch A
$ bash hamming.sh 'AAA' 'A?A'
0
$ touch B
$ bash hamming.sh 'AAA' 'A?A'
hamming.sh: line 23: [: too many arguments
0
```
Within `[...]` bare bash patterns will attempt to do [filename
expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion).
And as you never know what files your users will have, you need to quote the
right-hand side of `==` and `!=` within `[...]`

Within `[[...]]`, the right-hand side needs to be quoted as well, but for a
different reason: `==` and `!=` are not simply equality operators, they are
[_pattern matching_
operators](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b):
```bash
[[ $string == *[0-9]* ]] && echo "contains a digit"
```

In bash, prefer `[[...]]` over `[...]`. The double bracket conditional
command gives you more features (including regular expression matching), and
fewer surprises (particularly about unquoted variables). But as seen here,
there are still a couple of potential "gotcha"s in there.

<!-- ........................................................ -->
## grep

Instead of inventing your own way to parse the arguments, use the builtin `getopts`
command. There's a good [tutorial on the Bash Hackers wiki][getopts] and
[tons of examples on Stack Overflow][getopts-so].

[getopts]: https://wiki.bash-hackers.org/howto/getopts_tutorial
[getopts-so]: https://stackoverflow.com/search?q=%5Bbash%5D+getopts

<!-- -->

Using `getopts` we can replace all of lines 28-84 with
```bash
while getopts ":nlivxh" opt;  do
    case $opt in
        h) print_help ;;
        n) line_number=1 ;;
        l) files_with_matches=1 ;;
        i) ignore_case=1 ;;
        v) invert_match=1 ;;
        x) line_regexp=1 ;;
        *) echo "${0##*/}: unrecognized option '$OPTARG'"
           print_usage
           ;;
    esac
done
shift $((OPTIND - 1))   # these are the processed options
(( $# > 0 )) || print_usage
pattern=$1
files=( "${@:2}" )
```
`getopts` only handles single-letter arguments, so `-h` instead of `--help`. 
If you want long options, you can use `getopt` (here's [the example
script](https://salsa.debian.org/debian/util-linux/blob/master/misc-utils/getopt-parse.bash)
that ships with GNU getopt)

<!-- -->

Here's a `getopt` example that accepts long and short options:
```bash
temp=$(
    getopt \
        -o 'nlivx' \
        --long 'help,line-number,files-with-matches,ignore-case,invert-match,line-regexp' \
        -- "$@"
)
if (( $? != 0 )); then 
    echo "getopt error" >&2
    exit 1
fi
eval set -- "$temp"

while true; do
    case $1 in 
        '-n'|'--line-number')        line_number=1;        shift ;;
        '-l'|'--files-with-matches') files_with_matches=1; shift ;;
        '-i'|'--ignore-case')        ignore_case=1;        shift ;;
        '-v'|'--invert-match')       invert_match=1;       shift ;;
        '-x'|'--line-regexp')        line_regexp=1;        shift ;;
        '--help') print_help ;;
        '--')     shift; break ;;
        '--'*)    echo "unrecognized long option: '$1'" >&2
                  print_usage
                  ;;
        *) : ;; # unrecognized short options silently ignored
    esac
done

(( $# > 0 )) || print_usage
pattern=$1
files=( "${@:2}" )
(( ${#files[@]} > 0 )) || files+=("-")
```


<!-- ........................................................ -->
## acronym

You're not passing the last test. The problem with using unquoted variables
is that you're subjected to 
(i) [Word
Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting)
(which you want) 
but also (ii) [Filename
Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion)
(which you don't want).

In the last test, there's a `*` character standing alone as a word. When you do 
```bash
  for word in ${sanitised_sentence}
```
bash will:

* expand the variable
    ```bash
    for word in "Two * Words"
    ```
* then because the variable was not quoted, the value is split into words
    ```bash
    for word in Two * Words
    ```
* then because the variable was not quoted, each word is expanded into filenames
    ```bash
    for word in Two README.md acronym.sh acronym_test.sh Words
    ```

You can run the test with `bash -x acronym.sh "Two * Words"` to see what
bash is doing.  A couple of ways to deal with this:

1. turn off path expansion: see the `noglob` setting at [The Set
Builtin](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin)
2. read the words into an array with `read -a`
    ```bash
    read -r -a words <<< "${1//-/ }"
    for word in "${words[@]}"
    ```
    This approach allows you to keep the variables quoted at all times, so
    there won't be any expansion.

<!-- -->

To not worry about upper/lower case:

1. you can use a POSIX character class:
    ```bash
        if [[ $letter =~ [[:alpha:]] ]]
    ```
2. declare the `letter` variable with the lowercase attribute (anytime the
   variable is assigned to, the value will be lowercased -- see `help local`
   and `help declare` from a bash prompt for more details):
    ```bash
      local -l letter
       ...
        letter=${word:index:1}
        if [[ $letter =~ [a-z] ]]
    ```



---
# Miscellaneous notes to be organized

Proper indentation becomes essential for readability as the program grows.
[See what the Google style guide says about
indentation](https://google.github.io/styleguide/shellguide.html#s5-formatting).

<!-- -->

`eval` is generally considered dangerous. Here, it's pretty benign, but
there's a command specifically for declaring variables -- since we're using
this in a function, have to use `-g` option.
```bash
declare -g "$1=$((1 - $?))"
```

<!-- -->

Interesting reading: [Why is printf better than
echo?](https://unix.stackexchange.com/questions/65803/why-is-printf-better-than-echo)

<!-- -->

1. If you go to [3.2.4.2 Conditional
   Constructs](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
   in the bash manual and scroll down a bit, you'll find:

    > ((…))
    >
    >         (( expression ))
    >
    > The arithmetic expression is evaluated according to the rules
    > described below (see [Shell
    > Arithmetic](https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic)).
    > _If the value of the expression is non-zero, the return status is 0;
    > otherwise the return status is 1_. 

    My opinion, it's not a misuse to use ((...)) to do simple calculations
    and/or assign variables without using the return status. We do that all
    the time for other commands -- you wouldn't write 
    ```bash
    echo something || exit 1 # cannot write to stdout!
    ```

2. `[[` has the additional features of `=~` regex matching and `==` pattern
matching. And you can use `&&` and `||` and `()` to form more complex
tests.   

    `[[` does not do word splitting or filename expansion on unquoted
    variables, so I say that it has few surprises. Consider:
    ```
    $ var=""
    $ [[ -n $var ]] && echo "not empty" || echo empty
    empty
    $ [ -n $var ] && echo "not empty" || echo empty
    not empty
    ```
    Next example, depending on the contents of your current directory,
    you'll probably see this following result:
    ```
    $ var="*"
    $ [[ -n $var ]] && echo "not empty" || echo empty
    not empty
    $ [ -n $var ] && echo "not empty" || echo empty
    bash: [: too many arguments
    empty
    ```
    I can go into greater detail about why `[` gives incorrect results if you want.

    More details at [What is the difference between `test`, `[` and
    `[[`?](https://mywiki.wooledge.org/BashFAQ/031)

3. There is a difference between 
[regular expressions](https://www.gnu.org/software/gnulib/manual/html_node/posix_002dextended-regular-expression-syntax.html)
and [glob patterns](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching)

    Regular expression "quantifiers" `*` and `+` cannot appear without an
    immediately preceding "atom" to act upon:

    - The *regular expression* to match "zero or more of any character" is
    `.*`
    - The *glob pattern* to match "zero or more of any character" is `*`

    The glob pattern `[0-9]` will match exactly one character, a digit,
    because there are no wildcard `*`s in the pattern. If you  want to match
    a string *containing* a digit, you need to write "there may be some
    characters, then a digit, then there may be some characters" --
    `*[0-9]*`. This is why the bash extended pattern `+([0-9])` matches
    *only* one or more digits: there are no leading or trailing wildcards.
    The equivalent regex demands anchors: `^[0-9]+$`

    For the leap year exercise, we want to abort if it contains a
    non-digits. In the `year=''` scenario, the test `[[ $year == *[^0-9]*
    ]]` is insufficient because that pattern tests for: zero or more of any
    character, one non-digit, and zero or more of any character. If year is
    empty, _there is no non-digit_.  Therefore we also have to test if the
    variable is empty.
    
    In writing all this, I realize I may have bias *against* using regular
    expressions. Regex is a perfectly valid and powerful tool. I guess I
    just like using bash glob patterns because to me they feel more "native"
    to the shell. I wonder (but have no evidence) if the glob pattern
    matching performs better then regex matching.
    
    To conclude, I hope you learned something about glob patterns, and
    if you prefer to use regexes then don't let me stop you.

<!-- -->

### performance impact of subshells

https://exercism.io/mentor/solutions/d75f219017ad49dca16f68ab6772d7d2#discussion-post-599212

```bash
SECONDS=0
for (( count=0; count<100000; count++ )) ; do ( true ); done
echo $SECONDS

SECONDS=0
for (( count=0; count<100000; count++ )) ; do true; done
echo $SECONDS
```
```
69
1
```

### performance impact of string length

<details><summary>Obtaining the length of a string is a surprisingly
expensive operation in bash. With large strings and/or large loops,
performance can be significantly impacted.  Storing the length in a variable
helps significantly.  Click for benchmarks</summary><p>

```bash
$ printf -v string "%32767s" foo
$ time for ((i=0; i<${#string}; i++)); do :; done

real	0m14.807s
user	0m14.768s
sys	0m0.035s

$ len=${#string}
$ time for ((i=0; i<len; i++)); do :; done

real	0m0.189s
user	0m0.189s
sys	0m0.000s
```
Or if you can loop backwards, don't even need the variable. I imagine that
the 0.01 sec gain here is real: bash does not need to access the variable
contents for each iteration.
```bash
$ time for ((i = ${#string} - 1; i>=0; i--)); do :; done

real	0m0.178s
user	0m0.177s
sys	0m0.000s
```
But, a while-read loop is much faster than for if you need to iterate over
the characters of the string:
```bash
$ time while IFS= read -d "" -r -n 1 char; do 
    echo "$char"
  done < <(printf "%s" "$string") > /dev/null

real	0m0.603s
user	0m0.473s
sys	0m0.131s
```
Note the use of the printf process substitution. Using a `<<<"$string"`
here-string redirection adds a trailing newline.
</p></details>
