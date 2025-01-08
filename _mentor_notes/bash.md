# Bash

TOC
* [Tools](#tools)</br>
    * [Shellcheck](#shellcheck)<br>
    * [Shfmt](#shfmt)<br>
    * [Hyperfine](#hyperfine)<br>
* [How bash interprets command](#how-bash-interprets-command)<br>
* [Shebang](#shebang)<br>
* [Backticks](#backticks)<br>
* [Arithmetic](#arithmetic)<br>
* [Parameter Expansion](#parameter-expansion)<br>
    * [Special parameters](#special-parameters)<br>
* [Quoting](#quoting)<br>
* [Unset and Quoting](#unset-and-quoting)
* [Assignment](#assignment)<br>
* [Conditionals](#conditionals)<br>
* [Loops](#loops)<br>
* [Output](#output)<br>
* [Input](#input)<br>
* [Boolean Operators](#boolean-operators)<br>
* [Functions](#functions)<br>
    * [Passing an array to a function](#passing-an-array-to-a-function)<br>
* [Very rare and subtle mistakes](#very-rare-and-subtle-mistakes)<br>
* [Exit status](#exit-status)<br>
* Exercism
    * [Exercism/Philosophy](#exercismphilosophy)<br>
    * [Exercism/Testing](#exercismtesting)<br>
* [Miscellaneous notes to be organized](#miscellaneous-notes-to-be-organized)<br>
    * [performance impact of subshells](#performance-impact-of-subshells)
    * [performance impact of string length](#performance-impact-of-string-length)
    * [exploring character classes](#exploring-character-classes)
    * [bash dot files at startup](#bash-dot-files-at-startup)

# Syllabus

The bash learning track is a work in progress, but you can access the concept documents online:

1. [Commands and arguments](https://exercism.org/tracks/bash/concepts/commands-and-arguments)
1. [Variables](https://exercism.org/tracks/bash/concepts/variables)
1. [The Importance of Quoting](https://exercism.org/tracks/bash/concepts/quoting)
1. [Conditionals](https://exercism.org/tracks/bash/concepts/conditionals)
1. [Looping](https://exercism.org/tracks/bash/concepts/looping)
1. [Pipelines and Command Lists](https://exercism.org/tracks/bash/concepts/pipelines)

# Exercises

* [error-handling](#error-handling)
* [two-fer](#two-fer)
* [raindrops](#raindrops)
* [atbash-cipher](#atbash-cipher)
* [markdown](#markdown)
* [bob](#bob)
* [hamming](#hamming)
* [tournament](#tournament)
* [acronym](#acronym)
* [grep](#grep)
* [proverb](#proverb)
* [grains](#grains)
* [armstrong numbers](#armstrong-numbers)
* [difference of squares](#difference-of-squares)
* [word-count](#word-count)
* [anagram](#anagram)

Be sure to check out [the community solutions](https://exercism.io/tracks/bash/exercises/__SLUG__/solutions) to see other approaches.

This is a fine _shell_ solution. I challenge you to write a _bash_ solution, using no external tools.


## Exit status

The `exit 0` commands are not strictly necessary. Without them, the main
function will return the exit status of the last command executed (bc),
which will be zero. Then the script will exit with the return status of the
main function.

[3.3 Shell Functions](https://www.gnu.org/software/bash/manual/bash.html#Shell-Functions)

> When executed, the exit status of a function is the exit status of the last command executed in the body.



<!-- ============================================================ -->

## Tools

<!-- -->

A couple of tools you might be interested in:
* [shfmt](https://github.com/mvdan/sh#shfmt) for formatting shell scripts (or seeing how your style deviates from this tool's recommendations)
* [shellcheck](https://shellcheck.net) for linting. I'd recommend [acquiring the command line tool](https://github.com/koalaman/shellcheck#installing) for your OS.

<!-- -->

### Shellcheck

Linting: Shellcheck - https://github.com/koalaman/shellcheck

I recommend you paste this code into [https://shellcheck.net](https://shellcheck.net)
to get tips and further reading about improvements.

<!-- quick links -->

* [Shellcheck](https://shellcheck.net) warns [Argument mixes string and array](https://www.shellcheck.net/wiki/SC2145).
* [Shellcheck](https://shellcheck.net) warns [Assigning an array to a string](https://www.shellcheck.net/wiki/SC2124).
* [Shellcheck](https://shellcheck.net) warns [Useless echo? Instead of 'echo $(cmd)' just use cmd](https://www.shellcheck.net/wiki/SC2005).
* [Shellcheck](https://shellcheck.net) warns [Useless echo? Instead of 'cmd $(echo foo)' just use 'cmd foo'](https://www.shellcheck.net/wiki/SC2116).
* [Shellcheck](https://shellcheck.net) warns [Don't use variables in the printf format string.](https://www.shellcheck.net/wiki/SC2059).
* [Shellcheck](https://shellcheck.net) warns [Use "$@" (with quotes) to prevent whitespace problems.](https://www.shellcheck.net/wiki/SC2048).
* [Shellcheck](https://shellcheck.net) warns [Double quote array expansions to avoid re-splitting elements.](https://www.shellcheck.net/wiki/SC2068).
* [Shellcheck](https://shellcheck.net) warns [Quote parameters to `tr` to prevent glob expansion.](https://www.shellcheck.net/wiki/SC2060).
* [Shellcheck](https://shellcheck.net) warns [$/${} is unnecessary on arithmetic variables.](https://www.shellcheck.net/wiki/SC2004).
* [Shellcheck](https://shellcheck.net) warns [read without -r will mangle backslashes](https://www.shellcheck.net/wiki/SC2162).
* [Shellcheck](https://shellcheck.net) warns [Declare and assign separately to avoid masking return values](https://www.shellcheck.net/wiki/SC2155).
* [Shellcheck](https://shellcheck.net) warns [Double quote to prevent globbing and word splitting](https://www.shellcheck.net/wiki/SC2086).
* [Shellcheck](https://shellcheck.net) warns [Instead of `let expr`, prefer `(( expr ))`](https://www.shellcheck.net/wiki/SC2219).


### Shfmt

Formatting: shfmt - https://github.com/mvdan/sh

[shfmt](https://github.com/mvdan/sh#shfmt)

### Hyperfine

Benchmarking: [Hyperfine](https://github.com/sharkdp/hyperfine)

Example
```bash
hyperfine --warmup 10 --min-runs 100 \
    'bash implementation_1.sh args args args' \
    'bash implementation_2.sh args args args' \
    'bash implementation_3.sh args args args'
```

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

<!-- put another way -->

I'm mainly going to direct you to the bash manual.

* First read through [3.1.1 Shell Operation](https://www.gnu.org/software/bash/manual/bash.html#Shell-Operation), a high-level overview of how the shell interprets code
* Next, read [3.7.1 Simple Command Expansion](https://www.gnu.org/software/bash/manual/bash.html#Simple-Command-Expansion) (this is where it says "variable assignments [...] preceding the command name [...] are saved"
* Next read [3.5 Shell Expansions](https://www.gnu.org/software/bash/manual/bash.html#Shell-Expansions), noting the order that the expansions occur.
* Then dig in to [3.5.3 Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion), [3.5.7 Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting) and [3.5.8 Filename Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion) -- for the last 2, note "that did not occur within double quotes"

The bash manual can be a frustrating read. It's very dense, and literally every sentence is important, so it takes much re-reading for things to really sink in.

In my view, once you really understand these particular things (quoting, word splitting, globbing), you are well on your way to being comfortable in bash. Then you'll pick up other features more and more quickly.

---

## Testing

Make use of the test suite provided for you. See [Running the
Tests](https://exercism.io/tracks/bash/tests).

The tests will pass arguments to your program on the command line. You can
access them all at once with `"$@"` or idividually with `"$1"`, `"$2"`, etc.

<!-- ........................................................ -->
## Shebang

It is recommended that bash scripts start with
```bash
#!/usr/bin/env bash
``` 
The benefits include:
* instructing the OS to run your script with bash (if you make it executable)
* enabling syntax highlighting in your editor
* allowing people reading your code to know that this is specifically a bash program. 

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
[https://github.com/koalaman/shellcheck/wiki/SC2006](https://github.com/koalaman/shellcheck/wiki/SC2006)
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

Another quirk: if you use a variable in a function without declaring it local, then bash will move up the call stack looking for its nearest declaration:

```bash
f1() {
    local var=20
    echo "in f1, var=$var"
    f2
}
f2() {
    echo "in f2, var=$var"
}

var=10
echo "global var=$var"; f2
# => global var=10
# => in f2, var=10

echo "global var=$var"; f1
# => global var=10
# => in f1, var=20
# => in f2, var=20
```

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

<!-- -->

Functions will return the exit status of the last command performed. This
knowledge can make your code more concise. Instead of 
```bash
myfunc() {
    if [[ some condition ]]; then
        return 0
    else
        return 1
    fi
}
```
Since `[[` is a command and it has a 0/1 exit status, it's valid to write
```bash
myfunc() {
    [[ some condition ]]
}
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

<!-- -->

<!-- https://exercism.org/mentoring/discussions/33fe341522b740268e2c63766cbdc294 -->

(in reponse to: "I actually did it a bit strangely, I realize, where I pass the current bracket but not stack. In general, passing args is a bit verbose. What's a good practice here? Anything important to watch out in either case?")

bash has 2 variable scopes: global and local.

* global variables can be read/written anywhere in the file
* local variables (declared using the `local` or `declare` commands while inside a function). 
    * local variables can be read/written in the function **and any function called from it** (and further descendants), but not in the _parent_ scope

None of the variables here are `local`, so technically you don't _need_ to pass any of them (except `main "$@"` since functions get their own  positional parameters so there's no "global $1")

But of course there's a reason why functions with arguments were invented: global variables get hard to work with pretty quickly ("where does this variable come from?")

Another factor is that bash variables are kind of difficult to pass around. (I can talk more about this if you want.)

I don't know that there's one answer to the question. 

Personally, I try to code shell programs similarly like I would with other languages, mostly so that they are easier to read:

* I use `local` for variables that should be scoped to the function
* I try to restrict global variables to only constants, or perhaps some blob representing global state.

<!-- ........................................................ -->
## Arithmetic

bash can do arithmetic, you don't need to call out to `expr`. See 
[Arithmetic Expansion](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion)
and [Shell Arithmetic](https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic)
in the manual.
There is also an arithmetic conditional construct (analogous to the
string-oriented `[[...]]` conditional construct) -- see 
[Conditional Constructs](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down to `((...))` (there's no direct link).

<!-- -->

You can omit the `$` for variables inside an arithmetic expression.
See [this Shellcheck wiki entry](https://github.com/koalaman/shellcheck/wiki/SC2004) for details.

<!-- -->

<details><summary>You can omit the <code>$</code> for variables inside an arithmetic expression.
Click for details...</summary>

```bash
x=5
y=4
echo $((x * y))
# ......^...^
```
See [this Shellcheck wiki entry](https://github.com/koalaman/shellcheck/wiki/SC2004) for details.

This also applies to the index of a numerically indexed array:
```bash
for ((i = 0; i <= ${#ary[@]}; i++)); do
    echo "${ary[i]}"
    # ..........^
done
```
As well as the "offset" and "length" parts of the `${var:offset:length}`
expansion.

---
</details>

<!-- -->
<!-- ref https://exercism.org/mentoring/discussions/32d38bfe8bef4461a98837cd1d6587b1 -->

(re: student comment "remove $ which before 1st argument won't work. Still
need to keep $ sign in arithmetic parentheses")

Right, bash would have no way of knowing if you want the 1st arg or the number one.

The bash manual can be frustrating to read sometimes. The wording in it is
_very_ precise. The section on 
[Shell Arithmetic](https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic)
says:

> Within an expression, shell variables may also be referenced by name
> without using the parameter expansion syntax

"shell variables" actually has a specific meaning: it excludes positional
parameters and special parameters. 

- The [Shell Parameters](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters)
  section says:
    > A _parameter_ is an entity that stores values. [...] A _variable_ is a
    > parameter denoted by a `name`.

- And if we look in the
  [Definitions](https://www.gnu.org/software/bash/manual/bash.html#Definitions),
  a `name` is defined as:
    > A word consisting solely of letters, numbers, and underscores, and
    > beginning with a letter or underscore. `Name`s are used as shell
    > variable and function names. Also referred to as an `identifier`.

    It's the "beginning with a letter or underscore" bit that's important
    for variables being recognized in arithmetic expressions.

<!-- -->

<details><summary>You can assign to variables within an arithmetic
expression (click for details):</summary>

Instead of
```bash
total=$(( total + increment ))
```
You can write
```bash
(( total += increment ))
```
And if the increment is 1
```bash
(( total++ ))
```
</details>

<!-- -->

`((...))` is preferred over `let`. See [the let builtin
command](https://wiki.bash-hackers.org/commands/builtin/let) for details.
Also [the shellcheck wiki entry](https://github.com/koalaman/shellcheck/wiki/SC2219).

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

Within an arithmetic expression, bash allows for variable names without the `$`.
The offset and length parts of the `${var:offset:length}` parameter expansion are arithmetic expressions.

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


<!-- https://exercism.org/mentoring/discussions/781195efb84d49c9918ebf1de53f953a
 -->

Thanks for your comment. I'll try to be more clear.

`((...))` is the "arithmetic conditional" construct (see 
[Conditional Constructs](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down to `((...))` -- there's no direct link).
It's called a conditional construct because it can be used in an `if` command
```bash
if (( x == y + 1 )); then ...

# that looks nicer than
if [[ $x -eq $((y + 1)) ]]; then ...
```
In this context, we don't need to capture the _numerical result_ of the arithmetic expression, we only care about its _exit status_ 
* the `if` command executes the `then` commands if the condition is "true" (a zero exit status) or
* it executes the `else` commands if the condition is "false" (a non-zero exit status)

`$((...))` is the [arithmetic expansion](https://www.gnu.org/software/bash/manual/bash.html#Arithmetic-Expansion) construct.
The expression is evaluated and the numerical result is returned and can be captured in a variable.
```bash
z=$((z + 10))
```

Because variables can be modified in an arithmetic expression, it is cleaner (less "noisy") to use the conditional construct sometimes
```bash
((z += 10))
```

When we do it this way, we are not interested in the exit status.
But we have to be aware there is one.

What about `set -e`?

Some people like to use a bash "strict mode" 
([here](https://olivergondza.github.io/2019/10/01/bash-strict-mode.html) or
[here](http://redsymbol.net/articles/unofficial-bash-strict-mode/)).
In this "strict mode", if any command fails unexpectedly, then the script immediately aborts.
Often that's what you want.
Consider a script in a CI pipeline
```bash
#!/usr/bin/env bash
build_the_thing
deploy_the_thing
```
If the build step fails, you don't want to deploy it.

The intent of this "strict mode" is to force the programmer to think about what can go wrong and handle it properly
```bash
#!/usr/bin/env bash
build_the_thing && deploy_the_thing
```
or
```bash
#!/usr/bin/env bash
if ! build_the_thing; then
    echo "The build failed!" >&2
    exit 1
fi
if ! deploy_the_thing; then
    echo "Deployment failed!" >&2
    exit 1
fi
```

But bash has many many gotchas. Using the arithmetic conditional for "normal" math is one of them. You wouldn't expect `((count++))` to abort your script, but it can with `set -e` enabled.
This forces the programmer to write things like
```bash
((count++)) || true
```

There's a detailed discussion about this at [BashPitfalls](http://mywiki.wooledge.org/BashPitfalls#set_-euo_pipefail)

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

<!-- more detail -->

There are 3 particular variable expansions that are relevant:

* `${var:-default}` -- if $var is _unset or empty_, substitute "default"
* `${var:=default}` -- if $var is _unset or empty_, **assign** "default" to the var, and then expand to that value
* `${var:+alternate}` -- if $var is _set and not empty_, substitute "alternate"

<details><summary>Click for more details...</summary>

And 3 variations (that remove the colon only)
* `${var-default}` -- if $var is unset (only), substitute "default"
* `${var=default}` -- if $var is unset (only), assign  "default" to the var, and then expand to that value
* `${var+alternate}` -- if $var is set (only), substitute "alternate"

This demo shows the various effects:
```bash
unset var; echo ":->${var:-default1}"; echo " ->${var-default2}"
var="";    echo ":->${var:-default1}"; echo " ->${var-default2}"
var="x";   echo ":->${var:-default1}"; echo " ->${var-default2}"

# `:` is the "no-op" command, but it does process it's arguments
# see https://www.gnu.org/software/bash/manual/bash.html#index-_003a
unset var; : "${var:=default1}"; echo ":=>$var"
unset var; : "${var=default2}";  echo " =>$var"
var="";    : "${var:=default1}"; echo ":=>$var"
var="";    : "${var=default2}";  echo " =>$var"
var="x";   : "${var:=default1}"; echo ":=>$var"
var="x";   : "${var=default2}";  echo " =>$var"

unset var; echo ":+>${var:+alt1}"; echo " +>${var+alt2}"
var="";    echo ":+>${var:+alt1}"; echo " +>${var+alt2}"
var="x";   echo ":+>${var:+alt1}"; echo " +>${var+alt2}"
```
</details>

<!-- -->

Get out of the habit of using ALLCAPS variable names, leave those as
reserved by the shell. One day you'll write `PATH=something` and then
wonder why your script is broken (
[1](https://stackoverflow.com/q/27555060/7552), 
[2](https://stackoverflow.com/q/28310594/7552),
[3](https://unix.stackexchange.com/q/114596/4667) 
).

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

<!-- -->

<details><summary>Instead of trying to listing all the characters to remove
(it's easy to let a few characters slip through), think about want
characters you want to keep, and remove all the other ones. Click for an
example.</summary>
If you want to keep only letters and
numbers:
```bash
cleaned=${input//[^A-Za-z0-9]/}
```
or case insensitively
```bash
shopt -s nocasematch
cleaned=${input//[^A-Z0-9]/}
```
# or with a character class
cleaned=${input//[^[:alnum:]]/}
```
</details>

<!-- -->

<details><summary>bash can do most of what `tr` can do. Click for
details.</summary>

Look at 
[Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
and 
[Pattern Matching](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching).

For example:
```bash
x=$(echo "$y" | tr 'A' 'B')
```
can be
```bash
x=${y//A/B}
```
and
```bash
x=$(echo "$y" | tr -cd '[:alpha:][:blank:]')
```
can be
```bash
x=${y//[^[:alpha:][:blank:]]/}
```
Even `tr -s` can be done in bash, using "extended" patterns
```bash
y=aaaabaacaaada
echo "$y" | tr -s a -   # => -b-c-d-

echo "${y//a/-}"        # => ----b--c---d-

shopt -s extglob
echo ${y//+(a)/-}       # => -b-c-d-
```
</details>

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

Unquoted variables are subject to [word splitting](https://mywiki.wooledge.org/WordSplitting) and
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

<!-- -->

([responding to a question](https://exercism.io/mentor/solutions/fe8493cf90504101b886181c63e24667#discussion-post-910559))

> When whould I use/not use quotations around a variable?

That actually is a very deep question. I'll keep my answer brief, but feel free with followup questions.

Quotes are not required:

* on the right-hand side of a variable assignment
* inside `[[ ... ]]`
    * except on the right-hand side of `=~`, `==`, `!=` when you want the variable value to be used as literal text and not a pattern
* as the "word" of a [`case` statement](https://www.gnu.org/software/bash/manual/bash.html#index-case)
* as the "word" of a [Here String](https://www.gnu.org/software/bash/manual/bash.html#Here-Strings)
* when you _want_ the variable to undergo [Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting) and [Filename Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion)


<!-- ........................................................ -->
## Unset and Quoting

For the line `unset array[-1]`, Shellcheck warns 
["Quote arguments to unset so they're not glob expanded."](https://github.com/koalaman/shellcheck/wiki/SC2184)

Why is this? First consider how bash executes a command:
[3.7.1 Simple Command Expansion](https://www.gnu.org/software/bash/manual/bash.html#Simple-Command-Expansion).
Before executing the command, bash performs a series of expansions
([3.5 Shell Expansions](https://www.gnu.org/software/bash/manual/bash.html#Shell-Expansions)).
One of the expansions is [3.5.8 Filename Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion).
This is what makes `for f in *` iterate over the files in the current
directory. But it also makes `echo *` print the files instead of printing an
asterisk: if you want that, you need to quote the pattern: `echo '*'`

Similarly, `unset array[-1]` will attempt to expand that [valid glob
pattern](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching)
into matching files: that particular pattern can expand into one of "array-"
or "array1". If either of those files exist, then "array[-1]" will be
**replaced**, and the command then becomes `unset array1` (for instance),
certainly not what you intended. If neither of those files exist, the
_default_ behaviour of bash is to leave the pattern in place, and the last
element of the array will indeed be unset (however this behaviour can by
changed by [4.3.2 The Shopt Builtin](https://www.gnu.org/software/bash/manual/bash.html#The-Shopt-Builtin).
Like the `echo` example above, quoting prevents filename expansion: `unset
'array[-1]'`

<!-- ........................................................ -->
## Assignment

<details><summary>You can use the `+=` concatenating assignment operator:
click for details</summary>

These are equivalent:
```bash
foo="${foo}bar"
foo+="bar"
```
Full details at [Shell
Parameters](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters)
in the manual.
</details>

<!-- -->

Prepending a variable assignment to a command actually puts that variable in
the _environment_ of that command execution. See [3.7.1 Simple Command
Expansion](https://www.gnu.org/software/bash/manual/bash.html#Simple-Command-Expansion)
in the manual. 

A quick example:
```bash
MY_VAR='she said "hi"'  awk 'BEGIN {print ENVIRON["MY_VAR"]}'
echo "${MY_VAR:-variable not set}"
```
This can be very handy for passing a shell variable to a command body without having to jump through quoting hoops:
```bash
MY_VAR='she said "hi"'
awk 'BEGIN {print "'"${MY_VAR//\"/\\\"}"'"}'
```

(Yes, awk's `-v` option allows passing a shell variable into an awk variable:
```bash
awk -v greeting="$MY_VAR" 'BEGIN {print greeting}'
```
but some languages, like Expect, do not have such an option.)

<!-- ........................................................ -->
## Loops

<details><summary>It's not necessary to call out to <code>seq</code>: use
bash's builtin C-style for loop (click for details):</summary>

```bash
len=${#input}
for (( i = 0; i < len; i++ )); do ...
```

See [Looping
Constructs](https://www.gnu.org/software/bash/manual/bash.html#Looping-Constructs)
in the manual.
</details>

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

The [manual says](https://www.gnu.org/software/bash/manual/bash.html#index-_007b_005b),: "The words between the [[ and ]] do not undergo word splitting and filename expansion."
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

<details><summary>Click for
details about why <code>[</code> gives incorrect results if you want.</summary>

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
  and `[[` may or may not give an error (it has a more complicated parser).

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

---
</details>

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

<!--
https://exercism.io/mentor/solutions/4794e60d39154b298c232c7b814275f9?iteration_idx=1
-->

Referring to [the `[[...]]` documentation](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b), the `==` operator is a _pattern matching_ operator:
```bash
var=cat
[[ $var == c* ]] && echo "$var starts with c"
```
As the documentation (eventually) states:
> Any part of the pattern may be quoted to force the quoted portion to be matched as a string.

```bash
[[ $var == "c*" ]] && echo "$var is exactly c+star"
```
Since the words you're testing are not glob patterns, you're OK quoted or unquoted.

If the right-hand side is a variable:
* unquoted means pattern matching
* quoted means equality.

This is demonstrated in the hamming exercise.


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
$ help [[     | less
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
    [[ $string == $var   ]] && echo "string contains foo"
    [[ $string == "$var" ]] && echo "string is exactly '*foo*'"
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


<!--
    Way way more details:
    https://exercism.io/mentor/solutions/3246b249a635476d89a1cd9368c943f5?iteration_idx=4
-->

First, look up [3.2.5.2 Conditional Constructs](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs) in the manual. Then, at a bash prompt, do
```bash
help [
help test | less
help [[
help '(('
help let
```

`[` is an alias for the `test` command. In very early shells, these commands were not even built into the shell. If you type `type -a test [` you can see the artifacts of that history.

`[` is an ordinary command. That means the shell will perform all [3.5 Shell Expansions](https://www.gnu.org/software/bash/manual/bash.html#Shell-Expansions) on the arguments _before_ `[` executes. This means you have to be very vigilant about quoting variables inside `[...]`

`[[` is a shell **keyword**, not a command. That means the shell parses it differently than `[`
* `[[` can have special syntax (such as allowing `&&` and `||`)
* `[[` does not perform word splitting or filename expansion on variables, so quoting is less important.

Within `[[...]]` the `==` operator is not just string equality, it is actually a _pattern matching_ operator. We can write
```bash
[[ $var == a* ]] && echo "var starts with an a"
```
without worrying about the pattern expanding into a list of _files_ that start with "a". `[` uses `=` strictly as string equality because there's no way to specify a pattern without the shell expanding it to a list of files.

Also, `[[` implements regex matching with the `=~` operator.

IMO, there's no compelling reason to use `[` for bash code.

`((` is for arithmetic conditionals. `==` there is obviously numeric equality. bash arithmetic is integer-only. A nice feature here is that  the `$` can be omitted for variables and array elements.
```bash
x=10
a=( 5 -3 )
(( x == 5 )) && echo five || echo not five
(( a[0] * a[1] < 0 )) && echo negative || echo positive
```
This greatly increases readability for arithmetic expressions.

`$` is still required for positional parameters and for parameter expansions like `${#var}`

In addition to conditional expressions, `((` can be used to _modify_ variables:
```bash
i=1
while ((i < 20)); do
  echo $i
  ((i += i))
done
```
This is quite nice for readability: it takes out some of the syntax of `var=$((expression))` and allows more whitespace. It does not play well with `set -e` because `((` returns a non-zero exit status if the expression evaluates to zero:
```bash
bash -c '
  set -e
  x=1
  echo before
  (( x -= 1 ))  # script aborts here
  echo "after: this is not printed"
'
```

`((...))` is preferred over `let`, mostly for the same reasons `[[` is
preferred over `[`. See [the let builtin
command](https://wiki.bash-hackers.org/commands/builtin/let) for details.
Also [the shellcheck wiki entry](https://github.com/koalaman/shellcheck/wiki/SC2219).

<!-- -->



<!-- ........................................................ -->
## Boolean Operators

You have to be a bit careful with `A && B || C` -- C will execute if
**either** A **or** B fails.  With `if A; then B; else C; fi` the only time
C runs is if A fails, regardless of what happens with B.
But if B is an `echo` statement, that only fails if it can't write to the
output channel.

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
    - see [https://stackoverflow.com/a/26480210/7552](https://stackoverflow.com/a/26480210/7552)

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

In my view, Exercism is a site for programmers to pick up another language.
Exercism itself is not going to teach bash to you. Rather you are going to
learn bash yourself, using the exercises to gently guide your progress. The
chief benefits of Exercism as a learning tool are:

* having another programmer (the mentor) for asking questions.
* looking at how other programmers solved the same exercise, and discovering
  language features you didn't know about, or even seeing how other
  programmers use different coding paradigms.

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

<!-- -->

### set -E vs set -e

Note that `set -E` does not include `set -e`. Given this script that has an error in a function:
```bash
$ cat trap_test.sh
#!/usr/bin/env bash
trap 'echo "Error on line $LINENO. Exit code: $?" >&2' ERR
myfunc() {
    noSuchCommand
    echo OK
}
myfunc
```
then we can examine the combinations of -E and -e

1. neither specified: no trap, no early exit
    ```bash
    $ bash trap_test.sh; echo "exit status: $?"
    trap_test.sh: line 6: noSuchCommand: command not found
    OK
    exit status: 0
    ```
1. -e: early exit, no trap from the function
    ```bash
    $ bash -e trap_test.sh; echo "exit status: $?"
    trap_test.sh: line 6: noSuchCommand: command not found
    exit status: 127
    ```
1. -E, trap fired from error in function
    ```bash
    $ bash -E trap_test.sh; echo "exit status: $?"
    trap_test.sh: line 6: noSuchCommand: command not found
    Error on line 6. Exit code: 127
    OK
    exit status: 0
    ```
1. both early exit and trap/
    ```bash
    $ bash -eE trap_test.sh; echo "exit status: $?"
    trap_test.sh: line 6: noSuchCommand: command not found
    Error on line 6. Exit code: 127
    exit status: 127
    ```

<!-- ........................................................ -->
# Exercises

## error-handling

<details><summary>
There's a slightly different way to structure the code -- click for details:
</summary>

```bash
if (some error condition); then
    echo some error message >&2
    exit 1
fi

rest of code here
```

The "error and exit" can be extracted into a function (as a former perl
coder I like the function name "die"), and the code can look like:
```bash
die() { echo "$*" >&2; exit 1; }

(some SUCCESS condition) || die "some error message"
```
This style emulates an `assert` function that other languages have.

---
</details>

<!-- ........................................................ -->
## two-fer

<details><summary>In bash, prefer <code>[[...]]</code> over <code>[...]</code>. It's more powerful and less likely to act
in unexpected ways. Click for references:</summary>

* `[...]` is documented in the [`test` command in the manual](https://www.gnu.org/software/bash/manual/bash.html#index-test).
* `[[...]]` construct is documented in [Conditional Constructs](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b)

---
</details>

<!-- -->

There is a more concise way to manage the optional input. I suggest
looking into the `${var:-default}` form of parameter expansion [here in the
manual](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html).

<!-- -->

Generally, you should encapsulate the main body of your script in a `main`
function.  It encapsulates a chunk of logic in one function to encourage re-use.

It is good practice, and becomes more and more important as your programs get bigger.

<!-- -->

(discussion [about effect of IFS on expansion of positional parameters](https://exercism.io/mentor/solutions/645be831c6de43168f1fc7b6bdcab8ec))

Have a look at the Expansions section of the manual I linked earlier. It applies to the bats code too: when the test runs
```bash
bash two_fer.sh "John Smith" "Mary Ann"
```
then your script will receive `John Smith` as $1 and `Mary Ann` as $2 -- _**no quotes**_ (the last expansion is "quote removal")

In your 1st iteration, `main $@` is seen by bash as
```bash
main John Smith Mary Ann
```
And the main function receives `John` as $1

The 2nd iteration uses `main "$@"`. This is expanded as
```bash
main "John Smith" "Mary Ann"
```
And the main function receives `John Smith` as $1

When IFS is set to a double quote, **and** `$@` is unquoted, then each positional parameter will use IFS for word splitting. In this case, neither `John Smith` nor `Mary Add` contains double quotes, so the parameters are sent to main untouched.

However, if we forcibly inject double quotes into John Smith's name, then we can see the effect of IFS. This is using the 1st iteration code:
```bash
$ bash -x iteration1.sh  'John "Jonny Boy" Smith' 'Mary Ann'
+ IFS='"'
+ main 'John ' 'Jonny Boy' ' Smith' 'Mary Ann'
+ name='John '
+ [[ -z John  ]]
+ '[' 'John ' == '*' ']'
+ echo 'One for John , one for me.'
One for John , one for me.
```

We can observe:

* I sent 2 single-quoted arguments to the script
* the unquoted `$@` expanded the 2 arguments into 4, due to the presence of the literal double quotes
* the main function's $1 is `John ` with a trailing space -- the first quote-delimited "field" of the first program argument.

The key learning is: bash expands variable to their actual contents, and the _if the variable was not quoted_ then word splitting and filename expansion will be performed on the variable contents.

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

---
</details>

<!-- -->

<details><summary>Instead of putting an arithmetic expansion inside the
string-oriented <code>[[...]]</code>, you can use the arithmetic conditional
construct. Click for details.</summary>

Instead of this
```bash
if [[ $(( $1 % num )) == 0 ]]; then
```
you can do this
```bash
if (( $1 % num == 0 )); then
```
see [here in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down to `((...))`

---
</details>


<details><summary>Instead of putting an arithmetic expansion inside the
string-oriented <code>[...]</code>, you can use the arithmetic conditional
construct. Click for details.</summary>

---

Instead of this
```bash
if [ $(( $1 % num )) -eq 0 ]; then
```
you can do this
```bash
if (( $1 % num == 0 )); then
```
see [here in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
and scroll down to `((...))`
</details>


<!-- -->

<details><summary>For maximum brevity, you can use the 
<code>&&</code> control operator.
Click for details.</summary>

Instead of this
```bash
if some_conditional_command; then
    some_action
fi
```

you can do
```bash
some_conditional_command && some_action
```
see [here in the
manual](https://www.gnu.org/software/bash/manual/bash.html#Lists).

---
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

<!-- -->

It's really important to quote the arguments to `tr`: the "bare" bracket
expression will be used by bash for file expansion first. Try this:
```bash
bash -x bob.sh hello
touch ./e
bash -x bob.sh hello
rm e; touch ./o
bash -x bob.sh hello
touch ./w
bash -x bob.sh hello
```
 
<!-- ........................................................ -->
## grains

There are formulae to determine the results without looping. See:

* [summation](https://en.wikipedia.org/wiki/Summation) and
* [geometric progression](https://en.wikipedia.org/wiki/Geometric_progression)

<!-- -->

bash by itself can handle this: the values for individual squares and the total all fit in a 64-bit integer. The key is to print the values as _unsigned integers_.

<details><summary>Click for spoilers:</summary>
  
```sh
$ echo $((2 ** 64 - 1))
-1
$ printf '%d\n' $((2 ** 64 - 1))
-1
$ printf '%u\n' $((2 ** 64 - 1))
18446744073709551615
```
</details>

<!-- ........................................................ -->
## word-count

* http://forum.exercism.org/t/false-fails-on-bash-word-count-tests/11872
* https://unix.stackexchange.com/q/627474/4667
* https://exercism.org/tracks/bash/exercises/word-count/solutions/glennj
   
<!-- ........................................................ -->
## anagram

One thing to beware of: the variable in the for loop is unquoted.
This is necessary to iterate over the _words_ in the string.
But it leaves you exposed to filename expansion.

You'd expect this to return just one result, but there are two:
```bash
bash anagram.sh "shanagram." "grama.shan * grash-man"
```

You can turn off filename expansion with `set -f`.
See [The Set Builtin](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin) in the manual.

<!-- ........................................................ -->
## difference of squares

<details><summary>There are formulas to calculate the values, so 
loops aren't strictly needed. Click for hints...</summary>

The formulas for sums of powers of first _n_ numbers is given (and derived)
at [https://brilliant.org/wiki/sum-of-n-n2-or-n3/](https://brilliant.org/wiki/sum-of-n-n2-or-n3/)
</details>

<!-- -->

A nice way to implement a script that takes subcommands is:
* put the code for the subcommand in a function named after
  the subcommand,
* the `main` function dispatches to the functions with a
  `case` statement:
    ```bash
    case $1 in
        subcmd1 | subcmd2 | ... | subcmdN)
            "$1" "${@:2}"
            ;;
        *)  error "unknown subcommand $1" >&2
            exit 1
            ;;
    esac
    ```


<!-- ........................................................ -->
## armstrong numbers

Obtaining the length of a string is a surprisingly
expensive operation in bash. With large strings and/or large loops,
performance can be significantly impacted.  Storing the length in a variable
helps significantly. I've done 
[some benchmarking](https://github.com/glennj/exercism.io/blob/main/_mentor_notes/bash.md#performance-impact-of-string-length)
to demonstrate.

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
And as you never know what files your users will have, you need to quote
_all_ the arguments within `[...]`.

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
## proverb

<details><summary>This is a good exercise to learn about <em>indirect expansion</em>: click for details.</summary>

```bash
set -- foo bar baz
i=1 j=2
echo "For want of a ${!i} the ${!j} was lost."
# ....................^.........^
```
```none
For want of a foo the bar was lost.
```
See [Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion) in the manual.
</details>

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

<!-- -->

The duplicated code blocks indicate that things can be simpler. Some tips:
* while processing a file, you want lines that
    1. match the pattern and invert is _off_, OR
    1. don't match the pattern and invert is _on_

* after the `while getopts` loop, it's a good idea to `shift $((OPTIND - 1))`. Then the positional parameters that remain start at `$1`.

* the `nocasematch` shell option can be helpful.

* with regular expression matching, the `-x` option anchors the pattern at
  start and end of line.

<!-- ........................................................ -->
## acronym

<!-- -->
Although this is passing the tests, there is still a vulnerability.

On line 6, we leave the variable  unquoted to take advantage of [Word
Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting),
but omitting the quotes also enables [Filename
Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion).
You have added `*` to IFS, which is good, but that's not the only glob
pattern special character. Try this, and see if you're surprised by the
result:
```bash
touch {a,b,c}EADME.md
bash acronym.sh 'hello ?EADME.md'
```

<details><summary>There are a couple of ways to handle this. Click for spoilers:</summary>

1. turn off path expansion: see the `noglob` setting at [The Set
Builtin](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin)
2. read the words into an array with `read -a`
    ```bash
    read -r -a words <<< "${1//-/ }"
    for word in "${words[@]}"
    ```
    This approach allows you to keep the variables quoted at all times, so
    there won't be any expansion.

---
</details>
Be sure to check out [the community solutions](https://exercism.io/tracks/bash/exercises/acronym/solutions) to see other approaches.

<!-- -->
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

The variable is unquoted on line XYZ because you want to take advantage of 
[Word
Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting)
but
[Filename
Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion)
is still a factor: `*` is not the only special character to worry about:
```bash
$ touch {a,b,c}EADME.md
$ ls *EADME.md
README.md  aEADME.md  bEADME.md  cEADME.md
$ bash acronym.sh '?EADME.md'
RABC
```
<details><summary>There are a couple of ways to handle this: click for spoilers.</summary>

1. turn off path expansion: see the `noglob` setting at [The Set
Builtin](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin)
2. read the words into an array with `read -a`
    ```bash
    read -r -a words <<< "${1//-/ }"
    for word in "${words[@]}"
    ```
    This approach allows you to keep the variables quoted at all times, so
    there won't be any expansion.
</details>

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

<!-- -->

<details><summary>There's a more general way to extract the first letter
from a string, even if the word starts with non-letters. Click for
details.</summary>

Regular expression engines generally match "leftmost longest". For example
`[[ "aaBBBccBBBBBd" =~ B+ ]]` will match the first run of B's (leftmost) and
it will match all 3 of them (longest).

Additionally, the BASH_REMATCH array will hold the various captured parts:
```bash
$ [[ "aaBBBccBBBBBd" =~ (.)(B+) ]]
$ declare -p BASH_REMATCH
declare -ar BASH_REMATCH=([0]="aBBB" [1]="a" [2]="BBB")
```

So, putting that together, the first letter of a word:
```bash
word="__foo"
[[ $word =~ ([[:alpha:]]) ]]
echo ${BASH_REMATCH[1]} # => f
```
</details>


---
# Miscellaneous notes to be organized

Proper indentation becomes essential for readability as the program grows.
[See what the Google style guide says about
indentation](https://google.github.io/styleguide/shellguide.html#s5-formatting).


<!-- -->

Don't use nested functions unless the outer function is the _only_ place
where the inner function should be visible: see [this stackoverflow
answer](https://stackoverflow.com/a/8439070/7552)

<!-- -->

`eval` is generally considered dangerous. Here, it's pretty benign, but
there's a command specifically for declaring variables -- since we're using
this in a function, have to use `-g` option.
```bash
declare -g "$1=$((1 - $?))"
```

<!-- -->

Interesting reading: [Why is printf better than echo?](https://unix.stackexchange.com/questions/65803/why-is-printf-better-than-echo)

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

---
<!-- -->

Testing if a parameter is an integer:

1. as you are doing, with a regex:
    ```bash
    [[ $1 =~ ^[[:digit:]]+$ ]] && echo int
    ```
    Or, if negative numbers are allowed
    ```bash
    [[ $1 =~ ^[+-]?[[:digit:]]+$ ]] && echo int
    ```

1. with a "negative" regex: does it contain no non-digits?
    ```bash
    [[ ! $1 =~ [^[:digit:]] ]] && echo int
    ```

1. with bash "extended patterns"
    ```bash
    [[ $1 == +([[:digit:]]) ]] && echo int
    ```
    Or, if negative numbers are allowed
    ```bash
    [[ $1 == ?([+-])+([[:digit:]]) ]] && echo int
    ```
    Extended patterns are enabled with `shopt -s extglob`, except that they
    are used by default within `[[...]]`

All of those are using string comparision to test the value is a number.
I've seen some people force the parameter into an integer context and
rely on shell errors. I'm not a fan of this style
```bash
set -e
value=$(($1))
```
such as
```bash
$ set -- 42x
$ value=$(($1))
bash: 42x: value too great for base (error token is "42x")
```

---
<!--
    here-string vs pipe
    https://exercism.io/mentor/solutions/79bf2ee3402c45c792af8e89b15bbfb8
-->

It's a toss-up:

Here-string:
*  _may_ create a temp file on disk (I'm don't know how it's implemented)
*  implicitly adds a trailing newline to the data

Pipe:
* create subshells for both sides: process creation is probably faster than disk IO.
* because of subshells, variables do not persist: consider
    ```bash
    ary=(); echo foo bar | read -a ary; declare -p ary
    ```
    vs
    ```bash
    ary=(); read -a ary <<<"foo bar"; declare -p ary
    ```

<!-- -->

---
## performance impact of subshells

https://exercism.io/mentor/solutions/d75f219017ad49dca16f68ab6772d7d2#discussion-post-599212

```bash
# no subshell

$ time for (( count=0; count<100000; count++ )) ; do true; done

real	0m0.637s
user	0m0.632s
sys	0m0.001s

# with subshell
# ...................................................v......v
$ time for (( count=0; count<100000; count++ )) ; do ( true ); done


real	1m40.905s
user	0m30.578s
sys	1m12.693s
```

---
<!--
Obtaining the length of a string is a 
[surprisingly expensive operation](https://github.com/glennj/exercism.io/blob/main/_mentor_notes/bash.md#performance-impact-of-string-length)
in bash. With large strings and/or large loops, performance can be
significantly impacted.  Storing the length in a variable helps
significantly.  
-->

## performance impact of string length

Obtaining the length of a string is a surprisingly
expensive operation in bash. With large strings and/or large loops,
performance can be significantly impacted.  Storing the length in a variable
helps significantly.  

Demonstrating an empty loop, iterating over the string indices:
```bash
$ printf -v string "%32767s" foo
$ time for ((i = 0; i < ${#string}; i++)); do :; done

real	0m2.405s
user	0m2.400s
sys	0m0.003s

$ len=${#string}
$ time for ((i = 0; i < len; i++)); do :; done

real	0m0.193s
user	0m0.193s
sys	0m0.000s
```
Or if you can loop backwards, don't even need the variable. I imagine that
the 0.022 sec gain we see is significant: bash does not need to access the
variable contents for each iteration.
```bash
$ time for ((i = ${#string} - 1; i >= 0; i--)); do :; done

real	0m0.171s
user	0m0.170s
sys	0m0.000s
```
If you need to iterate over the characters of the string, a while-read loop
is _much_ faster than a for loop:
```bash
$ time for ((i = 0; i < len; i++)); do
     char="${string:i:1}"
     :
   done

real	0m7.062s
user	0m7.039s
sys	0m0.018s

$ time while IFS= read -d "" -r -n 1 char; do
    :
  done < <(printf "%s" "$string")

real	0m0.369s
user	0m0.335s
sys	0m0.034s
```
Note the use of the printf process substitution. Using a `<<<"$string"`
here-string redirection adds a trailing newline.

---

<!-- -->
<!-- https://exercism.io/mentor/solutions/46664b6978834cd7a3283e0be7a37335
    explaining the while read -n1 loop
-->

This is the idiomatic way to read lines from a file:
```bash
while IFS= read -r line; do ...; done < file
```
You might see
```bash
while read line; do ...; done < file
```
But that has 2 problems:
1. read will attempt to interpret backslashes as escape sequences without `-r`
2. Due to word splitting, any leading and/or trailing whitespace in the line will be missing unless you use `IFS=` or `IFS=""`

    * bash lets to set environment variables only for the duration of a command:
        ```bash
        VAR1=value1 VAR2=value some_command_here
        ```
        and then VAR1 and VAR2 will return to their previous values after the command returns.

Going back to my tricky code, the `-n` option for `read` specified the number of characters to read, not a whole line. I'm asking for one character at a time.

And I'm not reading from a file, I'm reading from a string. bash has a special 
[redirection](https://www.gnu.org/software/bash/manual/bash.html#Redirections)
for that: `<<<"$string"`. However, that adds a newline to the end of the string. I don't want to add a newline into the reversed string. The bash builtins `echo -n` and `printf '%s'` can output a string without a newline (see
[Why is printf better than echo?](https://unix.stackexchange.com/questions/65803/why-is-printf-better-than-echo)).

bash has a feature called [Process Substitution](https://www.gnu.org/software/bash/manual/bash.html#Process-Substitution)
-- you put some code inside `<(...)` and bash handles the output like it's a
file

That's where we get
```bash
while IFS= read -r -n1 line; do ...; done < <(printf '%s' "$1")
```

---
<!-- -->

(similar explanation about reading strings a character at a time,
demonstrating the addition of each interesting option)

For best practices, you need to quote the `$letter` variable on line 34.
While the answer is correct, try this to see what the shell does with `*`:
```sh
bash -x scrabble_score.sh 'a*b'
```

Not a big deal in this program, but it might become one when you're reading character by character: `<<<` adds a newline to the string.
What that means here: you get one extra iteration of the while loop.
To prevent that, use printf in a process substitution instead:
```sh
while read -rn1 letter; do
    ...
done < <(printf '%s' "${1^^}")
```

You almost always want to use the `-r` option for `read`.
Here's a [good explanation why][read-r]

<details><summary>And to read each character exactly as-is, you want to use:
<code>while IFS='' read -d '' -r -n 1 letter</code> -- Click here for a demo:</summary>
  
---

Given
```bash
line='a b\
c'
```
That variable starts with "a", space, "b"; it ends with "c"; and the
backslash+newline will be interpreted differently with different options.

First:
```bash
i=0
while read -n 1 char; do
    printf '%d\t%s\n' $((++i)) "${char@Q}"
done < <(printf '%s' "$line")
```
```none
1	'a'
2	''
3	'b'
4	'c'
```
The space was read as an empty string, and backslash+newline was interpreted
as literally nothing. This is the bash line-continuation in action.

Next, add `-r`
```bash
i=0
while read -r -n 1 char; do
    printf '%d\t%s\n' $((++i)) "${char@Q}"
done < <(printf '%s' "$line")
```
```none
1	'a'
2	''
3	'b'
4	'\'
5	''
6	'c'
```
That makes the backslash be taken as a plain character. The newline is still
an empty string.

Next, add `IFS=''` or `IFS=` for short
```bash
i=0
while IFS= read -r -n 1 char; do
    printf '%d\t%s\n' $((++i)) "${char@Q}"
done < <(printf '%s' "$line")
```
```none
1	'a'
2	' '
3	'b'
4	'\'
5	''
6	'c'
```
Now the space is actually a space, but newline is still empty.

Finally, add `-d ''` -- this changes how `read` handles end-of-line
* by default, end-of-line is a newline (hence the empty string above where the newline appears in the string)
* `-d ''` uses `\0` (the null byte) as end-of-line. 
  
```bash
i=0
while IFS= read -d '' -r -n 1 char; do
    printf '%d\t%s\n' $((++i)) "${char@Q}"
done < <(printf '%s' "$line")
```
```none
1	'a'
2	' '
3	'b'
4	'\'
5	$'\n'
6	'c'
```
At last, every character is read individually.

Demonstrating the extra loop iteration when using a here-string:
```bash
i=0
while IFS= read -d '' -r -n 1 char; do
    printf '%d\t%s\n' $((++i)) "${char@Q}"
done <<< "$line"
```
```none
1	'a'
2	' '
3	'b'
4	'\'
5	$'\n'
6	'c'
7	$'\n'
```

---
</details>

[read-r]: https://stackoverflow.com/a/26480210/7552


<!-- -->

---
## exploring character classes

Ref [3.5.8.1 Pattern Matching](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching),
let's see which ASCII characters are contained in which POSIX character class:

```bash
#!/usr/bin/env bash

# `chr 65` == "A"
chr() {
    printf "\x$(printf "%x" "$1")"
}

classes=(
    alpha alnum upper lower word 
    digit xdigit
    space blank
    punct
    cntrl graph print
)

classify() {
    local a=$1 char=$2
    printf "%d\t%03o\t%02x\t%s" $a $a $a "${char@Q}"
    for cls in "${classes[@]}"; do
        patt="[[:$cls:]]"
        printf "\t"
        [[ $char == $patt ]] && printf Y || printf .
    done
    echo
}

header=$(
    printf "dec\toct\thex\tchar"
    for cls in "${classes[@]}"; do printf "\t%s" $cls; done
)

for j in {0..3}; do
    echo "$header"
    for i in {0..31}; do
        a=$(( 32 * j + i ))

        # command substitution strips trailing newlines,
        # which is a problem when the function returns a newline.
        ((a==10)) && char=$'\n' || char=$(chr $a)

        classify $a "$char"
    done
done

# some non-ASCII chars
echo "$header"
chars=(
        · ≤ € ¥ ¿ ¡     # math, currency, punct
        à á â ã ä å æ   # accented letters
        ♢ ♠ ♡ ♣         # card suits
        あ い う え お  # Hiragana letters
        ア イ ウ エ オ  # Katakana letters
        ﬀ ﬁ ﬂ           # Latin ligatures
)
for char in "${chars[@]}"; do classify 0 "$char"; done
```

From this, we can observe:

* the `[[:alpha:]]` class consists of `[[:lower:][:upper:]]`
* the `[[:alnum:]]` class consists of `[[:alpha:][:digit:]]`
* the `[[:word:]]` class consists of `[[:alnum:]_]`
* underscore (octal 137) is both `[[:word:]]` and `[[:punct:]]` 
* the `[[:graph:]]` class consists of `[[:alnum:][:punct:]]`
* the `[[:print:]]` class consists of `[[:graph:] ]` -- just `space` (octal 040) not any other whitespace.

output
```none
dec	oct	hex	char	alpha	alnum	upper	lower	word	digit	xdigit	space	blank	punct	cntrl	graph	print
0	000	00	''	.	.	.	.	.	.	.	.	.	.	.	.	.
1	001	01	$'\001'	.	.	.	.	.	.	.	.	.	.	Y	.	.
2	002	02	$'\002'	.	.	.	.	.	.	.	.	.	.	Y	.	.
3	003	03	$'\003'	.	.	.	.	.	.	.	.	.	.	Y	.	.
4	004	04	$'\004'	.	.	.	.	.	.	.	.	.	.	Y	.	.
5	005	05	$'\005'	.	.	.	.	.	.	.	.	.	.	Y	.	.
6	006	06	$'\006'	.	.	.	.	.	.	.	.	.	.	Y	.	.
7	007	07	$'\a'	.	.	.	.	.	.	.	.	.	.	Y	.	.
8	010	08	$'\b'	.	.	.	.	.	.	.	.	.	.	Y	.	.
9	011	09	$'\t'	.	.	.	.	.	.	.	Y	Y	.	Y	.	.
10	012	0a	$'\n'	.	.	.	.	.	.	.	Y	.	.	Y	.	.
11	013	0b	$'\v'	.	.	.	.	.	.	.	Y	.	.	Y	.	.
12	014	0c	$'\f'	.	.	.	.	.	.	.	Y	.	.	Y	.	.
13	015	0d	$'\r'	.	.	.	.	.	.	.	Y	.	.	Y	.	.
14	016	0e	$'\016'	.	.	.	.	.	.	.	.	.	.	Y	.	.
15	017	0f	$'\017'	.	.	.	.	.	.	.	.	.	.	Y	.	.
16	020	10	$'\020'	.	.	.	.	.	.	.	.	.	.	Y	.	.
17	021	11	$'\021'	.	.	.	.	.	.	.	.	.	.	Y	.	.
18	022	12	$'\022'	.	.	.	.	.	.	.	.	.	.	Y	.	.
19	023	13	$'\023'	.	.	.	.	.	.	.	.	.	.	Y	.	.
20	024	14	$'\024'	.	.	.	.	.	.	.	.	.	.	Y	.	.
21	025	15	$'\025'	.	.	.	.	.	.	.	.	.	.	Y	.	.
22	026	16	$'\026'	.	.	.	.	.	.	.	.	.	.	Y	.	.
23	027	17	$'\027'	.	.	.	.	.	.	.	.	.	.	Y	.	.
24	030	18	$'\030'	.	.	.	.	.	.	.	.	.	.	Y	.	.
25	031	19	$'\031'	.	.	.	.	.	.	.	.	.	.	Y	.	.
26	032	1a	$'\032'	.	.	.	.	.	.	.	.	.	.	Y	.	.
27	033	1b	$'\E'	.	.	.	.	.	.	.	.	.	.	Y	.	.
28	034	1c	$'\034'	.	.	.	.	.	.	.	.	.	.	Y	.	.
29	035	1d	$'\035'	.	.	.	.	.	.	.	.	.	.	Y	.	.
30	036	1e	$'\036'	.	.	.	.	.	.	.	.	.	.	Y	.	.
31	037	1f	$'\037'	.	.	.	.	.	.	.	.	.	.	Y	.	.
```
```none
dec	oct	hex	char	alpha	alnum	upper	lower	word	digit	xdigit	space	blank	punct	cntrl	graph	print
32	040	20	' '	.	.	.	.	.	.	.	Y	Y	.	.	.	Y
33	041	21	'!'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
34	042	22	'"'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
35	043	23	'#'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
36	044	24	'$'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
37	045	25	'%'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
38	046	26	'&'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
39	047	27	\'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
40	050	28	'('	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
41	051	29	')'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
42	052	2a	'*'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
43	053	2b	'+'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
44	054	2c	','	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
45	055	2d	'-'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
46	056	2e	'.'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
47	057	2f	'/'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
48	060	30	'0'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
49	061	31	'1'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
50	062	32	'2'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
51	063	33	'3'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
52	064	34	'4'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
53	065	35	'5'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
54	066	36	'6'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
55	067	37	'7'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
56	070	38	'8'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
57	071	39	'9'	.	Y	.	.	Y	Y	Y	.	.	.	.	Y	Y
58	072	3a	':'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
59	073	3b	';'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
60	074	3c	'<'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
61	075	3d	'='	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
62	076	3e	'>'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
63	077	3f	'?'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
```
```none
dec	oct	hex	char	alpha	alnum	upper	lower	word	digit	xdigit	space	blank	punct	cntrl	graph	print
64	100	40	'@'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
65	101	41	'A'	Y	Y	Y	.	Y	.	Y	.	.	.	.	Y	Y
66	102	42	'B'	Y	Y	Y	.	Y	.	Y	.	.	.	.	Y	Y
67	103	43	'C'	Y	Y	Y	.	Y	.	Y	.	.	.	.	Y	Y
68	104	44	'D'	Y	Y	Y	.	Y	.	Y	.	.	.	.	Y	Y
69	105	45	'E'	Y	Y	Y	.	Y	.	Y	.	.	.	.	Y	Y
70	106	46	'F'	Y	Y	Y	.	Y	.	Y	.	.	.	.	Y	Y
71	107	47	'G'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
72	110	48	'H'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
73	111	49	'I'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
74	112	4a	'J'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
75	113	4b	'K'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
76	114	4c	'L'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
77	115	4d	'M'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
78	116	4e	'N'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
79	117	4f	'O'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
80	120	50	'P'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
81	121	51	'Q'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
82	122	52	'R'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
83	123	53	'S'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
84	124	54	'T'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
85	125	55	'U'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
86	126	56	'V'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
87	127	57	'W'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
88	130	58	'X'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
89	131	59	'Y'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
90	132	5a	'Z'	Y	Y	Y	.	Y	.	.	.	.	.	.	Y	Y
91	133	5b	'['	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
92	134	5c	'\'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
93	135	5d	']'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
94	136	5e	'^'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
95	137	5f	'_'	.	.	.	.	Y	.	.	.	.	Y	.	Y	Y
```
```none
dec	oct	hex	char	alpha	alnum	upper	lower	word	digit	xdigit	space	blank	punct	cntrl	graph	print
96	140	60	'`'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
97	141	61	'a'	Y	Y	.	Y	Y	.	Y	.	.	.	.	Y	Y
98	142	62	'b'	Y	Y	.	Y	Y	.	Y	.	.	.	.	Y	Y
99	143	63	'c'	Y	Y	.	Y	Y	.	Y	.	.	.	.	Y	Y
100	144	64	'd'	Y	Y	.	Y	Y	.	Y	.	.	.	.	Y	Y
101	145	65	'e'	Y	Y	.	Y	Y	.	Y	.	.	.	.	Y	Y
102	146	66	'f'	Y	Y	.	Y	Y	.	Y	.	.	.	.	Y	Y
103	147	67	'g'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
104	150	68	'h'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
105	151	69	'i'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
106	152	6a	'j'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
107	153	6b	'k'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
108	154	6c	'l'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
109	155	6d	'm'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
110	156	6e	'n'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
111	157	6f	'o'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
112	160	70	'p'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
113	161	71	'q'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
114	162	72	'r'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
115	163	73	's'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
116	164	74	't'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
117	165	75	'u'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
118	166	76	'v'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
119	167	77	'w'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
120	170	78	'x'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
121	171	79	'y'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
122	172	7a	'z'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
123	173	7b	'{'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
124	174	7c	'|'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
125	175	7d	'}'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
126	176	7e	'~'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
127	177	7f	$'\177'	.	.	.	.	.	.	.	.	.	.	Y	.	.
```
```none
dec	oct	hex	char	alpha	alnum	upper	lower	word	digit	xdigit	space	blank	punct	cntrl	graph	print
0	000	00	'·'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'≤'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'€'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'¥'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'¿'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'¡'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'à'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'á'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'â'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'ã'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'ä'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'å'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'æ'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'♢'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'♠'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'♡'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'♣'	.	.	.	.	.	.	.	.	.	Y	.	Y	Y
0	000	00	'あ'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'い'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'う'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'え'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'お'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'ア'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'イ'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'ウ'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'エ'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'オ'	.	.	.	.	.	.	.	.	.	.	.	Y	Y
0	000	00	'ﬀ'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'ﬁ'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
0	000	00	'ﬂ'	Y	Y	.	Y	Y	.	.	.	.	.	.	Y	Y
```

<!-- -->

---
## bash dot files at startup

bash processes several files when it starts up. 
See [6.2 Bash Startup Files](https://www.gnu.org/software/bash/manual/bash.html#Bash-Startup-Files) in the manual.

The trick is that each of these files may source other files, and tracing
them manually can be a pain. To inspect (most of) them, this is a neat
trick:

```bash
$ function source { for f do echo "> ${f@Q}"; done; builtin source "$@"; }
$ function .      { for f do echo "> ${f@Q}"; done; builtin . "$@"; }
$ export -f source .
$ bash -li
```
You may see differences between `bash -l`, `bash -i` and `bash -li`.
