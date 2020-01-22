# Bash

[Testing](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#testing)<br>
[Shebang](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#shebang)<br>
[Backticks](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#backticks)<br>
[Arithmetic](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#arithmetic)<br>
[Variables](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#variables)<br>
[Quoting](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#quoting)<br>
[Assignment](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#assignment)<br>
[Conditionals](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#conditionals)<br>
[Loops](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#loops)<br>
[Output](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#output)<br>
[Input](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#input)<br>
[Very rare and subtle mistakes](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#very-rare-and-subtle-mistakes)<br>
[Exercism/Philosophy](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#exercismphilosophy)<br>
[Miscellaneous notes to be organized](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#miscellaneous-notes-to-be-organized)<br>

Exercises

* [two-fer](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#two-fer)
* [raindrops](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#raindrops)
* [atbash-cipher](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#atbash-cipher)
* [markdown](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#markdown)
* [bob](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#bob)
* [hamming](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#hamming)
* [acronym](https://github.com/glennj/exercism.io/blob/master/_mentor_notes/bash.md#acronym)



---
## Testing

Make use of the test suite provided for you. See [Running the Tests](https://exercism.io/tracks/bash/tests).

<!-- ........................................................ -->
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

`((...))` is preferred over `let`. See [the let builtin command](https://wiki-dev.bash-hackers.org/commands/builtin/let) for details.

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

Consider the `${var:-default value}` form of [Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion).

<!-- -->

Get out of the habit of using ALLCAPS variable names, leave those as reserved by the shell. One day you'll write `PATH=something` and then [wonder why your script is broken](https://stackoverflow.com/q/28310594/7552).

<!-- ........................................................ -->
## Quoting

Unquoted variables are subject to [word splitting](https://mywiki.wooledge.org/WordSplitting) and [glob](https://mywiki.wooledge.org/glob) expansion.

<!-- ........................................................ -->
## Assignment

You can use the `+=` concatenating assignment operator: these are equivalent:
```bash
foo=${foo}bar
foo+=bar
```

<!-- ........................................................ -->
## Loops

You don't need to call out to `seq`: use a builtin bash C-style for loop:
```bash
len=${#input}
for (( i = 0; i < len; i++ )); do ...
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

<!-- -->

It is very important to quote your variables. Try this:
```bash
var="  *"
echo $var
# then
echo "$var"
```
There are even [Security implications of forgetting to quote a variable in bash/POSIX shells](https://unix.stackexchange.com/questions/171346/security-implications-of-forgetting-to-quote-a-variable-in-bash-posix-shells)

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

<!-- ........................................................ -->
# Exercism/Philosophy

_(Responding to a comment about using bash features vs external tools)_

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

<!-- -->

You can use the `+=` concatenating assignment operator: these are equivalent:
```bash
foo=${foo}bar
foo+=bar
```

<!-- -->

Instead of looping over all the numbers from 1 to _num_, you only
need to test the remainder of 3, 5 and 7.

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
    echo 'Calm down, I know what I'm doing!'
else if input is just yelling
    echo 'Whoa, chill out!'
else if input is just a question
    echo 'Sure.'
else any other input
    echo 'Whatever.'
```

Notice that yelling and question appear twice? That indicates we should try to put that in some reusable form, like a variable or a function.

Let's look at these factors and see how to satisfy them:
* silence is easy, $input is empty
* question is simple too, $input ends with a question mark
* yelling can be described as: $input contains upper case letters but no lower case letters. We don't care about the presence or absence of digits.

Before continuing, note that the `==` operator in bash is not a string equality operator, it is a _pattern matching_ operator (see [here in the manual](https://www.gnu.org/software/bash/manual/bash.html#index-_005b_005b)). So we can use `==` for our "contains an upper" test. Yelling can be tested thusly:
```bash
[[ $input == *[[:upper:]]* ]] && [[ $input != *[[:lower:]]* ]]
```
[Bash patterns](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching) allow us to do a lot without having to use regular expressions.

About that "reusable form" remark. How to achieve that?

First, note the syntax of `if` (bash has a really useful interactive `help` system)
```bash
$ help if
if: if COMMANDS; then COMMANDS; [ elif COMMANDS; then COMMANDS; ]... [ else COMMANDS; ] fi
    Execute commands based on conditional.

    The `if COMMANDS' list is executed.  If its exit status is zero, then the ...
```
See that after the `if` keyword, bash wants to see COMMANDS. There's nothing specific about `[` or `[[` or `((` -- those are just builtin commands. We could put any any list/pipeline of commands in there, and `if` branches based on the exit status.

I mentioned variables. Here's a technique I like to use that some people don't like. `true` and `false` are bash builtin commands that return the expected exit status. So you can do this:
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

<!-- ........................................................ -->
## acronym

You're not passing the last test. The problem with using unquoted variables
is that you're subjected to 
(i) [Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting) (which you want) 
but also (ii) [Filename Expansion](https://www.gnu.org/software/bash/manual/bash.html#Filename-Expansion)
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
bash is doing. 

Read in the manual about 
[The Set Builtin](https://www.gnu.org/software/bash/manual/bash.html#The-Set-Builtin) 
to disable filename expansion.


---
# Miscellaneous notes to be organized

<!-- -->

Interesting reading: [Why is printf better than echo?](https://unix.stackexchange.com/questions/65803/why-is-printf-better-than-echo)

<!-- -->

1. If you go to [3.2.4.2 Conditional Constructs](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs) in the bash manual and scroll down a bit, you'll find:

    > ((…))
    >
    >         (( expression ))
    >
    > The arithmetic expression is evaluated according to the rules described below (see [Shell Arithmetic](https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic)). _If the value of the expression is non-zero, the return status is 0; otherwise the return status is 1_. 

    My opinion, it's not a misuse to use ((...)) to do simple calculations and/or assign variables without using the return status. We do that all the time for other commands -- you wouldn't write 
    ```bash
    echo something || exit 1 # cannot write to stdout!
    ```

2. `[[` has the additional features of `=~` regex matching and `==` pattern
matching. And you can use `&&` and `||` and `()` to form more complex
tests.   

    `[[` does not do word splitting or filename expansion on unquoted variables, so I say that it has few surprises. Consider:
    ```
    $ var=""
    $ [[ -n $var ]] && echo "not empty" || echo empty
    empty
    $ [ -n $var ] && echo "not empty" || echo empty
    not empty
    ```
    Next example, depending on the contents of your current directory, you'll probably see this following result:
    ```
    $ var="*"
    $ [[ -n $var ]] && echo "not empty" || echo empty
    not empty
    $ [ -n $var ] && echo "not empty" || echo empty
    bash: [: too many arguments
    empty
    ```
    I can go into greater detail about why `[` gives incorrect results if you want.

    More details at [What is the difference between `test`, `[` and `[[`?](https://mywiki.wooledge.org/BashFAQ/031)

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
    non-digits. In the `year=''` scenario, the test `[[ $year == *[^0-9]* ]]` is insufficient because
    that pattern tests for: zero or more of any character, one non-digit, and
    zero or more of any character. If year is empty, _there is no non-digit_.
    Therefore we also have to test if the variable is empty.
    
    In writing all this, I realize I may have bias *against* using regular
    expressions. Regex is a perfectly valid and powerful tool. I guess I
    just like using bash glob patterns because to me they feel more "native"
    to the shell. I wonder (but have no evidence) if the glob pattern
    matching performs better then regex matching.
    
    To conclude, I hope you learned something about glob patterns, and
    if you prefer to use regexes then don't let me stop you.
