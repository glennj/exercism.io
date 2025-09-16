# Tcl

TOC
* [Welcome back to Tcl](#welcome-back-to-tcl)
* [Expressions](#expressions)
* [Exception Handling](#exception-handling)
* [indices](#indices)
* [uplevel and upvar](#uplevel-and-upvar)
* [Namespace variables](#namespace-variables)
* [TclOO Notes](#tcloo-notes)
* [Optimizations](#optimizations)
* [Links and references](#links-and-references)

Exercises
* [error-handling](#error-handling)
* [word-count](#word-count)
* [clock](#clock)
* [two-bucket](#two-bucket)

---
<!-- #################################################### -->

## Welcome back to Tcl

To catch up with Tcl 8.6, you might want to:
* refresh your memory about 
  [the dodekalogue][man-Tcl] and the
  [available commands][man-contents]
* read about [OO programming in Tcl](https://www.magicsplat.com/articles/oo.html)

---
<!-- #################################################### -->

## Exception Handling

Tcl 8.6 introduced the [`try` command][man-try].
This encapsulates a lot of the uses of `catch`

```tcl
try {
    expr {$a / $b}
} trap {ARITH DIVZERO} {
    puts "division by zero"
}
```
Now, where is "ARITH DIVZERO" documented? I think only in the source code,
but we can ask tcl to show us:
```tcl
$ tclsh
% set a 1; set b 0
% expr {$a / $b}
divide by zero

% set errorInfo     ;# this is the stack trace
divide by zero
    while executing
"expr {$a / $b}"

% set errorCode     ;# this is what we're after
ARITH DIVZERO {divide by zero}
```
So the error to be trapped is `lrange $errorCode 0 end-1`

---
<!-- #################################################### -->

## Expressions

The first argument to [`if`][man-if] and [`while`][man-while] and the 2nd argument to [`for`][man-for] are already handled by (the internal guts of) `expr`.
It's not necessary to explicitly call [`expr`][man-expr]:

```tcl
for {set x 1} {$x <= [expr [llength $prec_row] - 1]} {incr x} {
#
for {set x 1} {$x <= [llength $prec_row] - 1} {incr x} {
```

There's a micro-optimization to make there: you don't need to recalcuate the list length for every iteration
```tcl
set len [llength $prec_row]
for {set x 1} {$x <= $len - 1} {incr x} {
```


## Welcome back to Tcl

<!-- #################################################### -->

### Further reading

* [Errors management](https://wiki.tcl-lang.org/page/Errors+management)
* [try](https://wiki.tcl-lang.org/page/try)
* the [`errorCode` global
  variable](https://www.tcl-lang.org/man/tcl/TclCmd/tclvars.htm#M12)
* the [Child Status
  section](https://wiki.tcl-lang.org/page/exec#1a39e854c66866d4b4567dcb8126b8f0914bdab191eea3a4812f3ca8d9d2bff3)
  of the [`exec` wiki page](https://wiki.tcl-lang.org/page/exec)
* the [original TIP for try/catch/finally](https://core.tcl-lang.org/tips/doc/trunk/tip/329.md)


---
<!-- #################################################### -->

## indices

It's not strictly necessary to use `expr` here: See the [String
Indices](http://www.tcl-lang.org/man/tcl8.6/TclCmd/string.htm#M54) section
of the string man page (lindex indices use the same rules).

---
<!-- #################################################### -->

## uplevel and upvar

When you have a proc that receives a script to evaluate, you want to do the
evaluation in _the **caller's** stack frame_.  You have to use the
[`uplevel`][man-uplevel] command
to perform the script in the same context it was defined.
I can provide more details if you want.

As an example, if you're writing a `do ... while ...` procedure, you might
think you could:
```tcl
proc do {script whileKeyword condition} {
    eval $script
    while {[expr $condition]} {
        eval $script
    }
}
```
And then run it like:
```tcl
set mylist {}
set n 3
do {lappend mylist $n; incr n -1} while {$n > 0}
```
But this happens:
```none
can't read "n": no such variable
```
That's because the `do` proc has no variable `n`. But the caller does.


```tcl
proc do {script whileKeyword condition} {
    uplevel 1 $script
    while {[uplevel 1 [list expr $condition]]} {
        uplevel 1 $script
    }
}

set n 3
do {lappend mylist $n; incr n -1} while {$n > 0}
puts $mylist  ;# => 3 2 1
puts $n       ;# => 0
```

<!-- #################################################### -->

A companion to `uplevel` is
[`upvar`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/upvar.htm)
that connects variables in different stack frames. Think of upvar as
creating an "alias" for the variable.

This is invaluable when you're implementing control procs that use a
variable to, say, iterate over a list (like `foreach` does). For example:
```tcl
proc select {varName elements condition} {
    upvar 1 $varName elem
    set selected {}
    foreach elem $elements {
        if {[uplevel 1 [list expr $condition]]} {
            lappend selected $elem
        }
    }
    return $selected
}

set mylist {1 2 3 4 5 6 7}
set evens [select num $mylist {$num % 2 == 0}]
puts $evens    ;# => 2 4 6
```

In the select, 
* `upvar` connects the local `elem` variable to the caller's `num` variable, 
* foreach sets the value of `elem` which also sets `num` in the caller, 
* and the "even-ness" test (that explicitly refers to `$num`) is performed
  in the caller's context

<!-- #################################################### -->

That `select` proc can use 
[`lmap`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/lmap.htm)
to be a little more concise:
```tcl
proc select {varName elements condition} {
    upvar 1 $varName elem
    lmap elem $elements {
        if {[uplevel 1 [list expr $condition]]} {
            set elem
        } else continue
    }
}
```

---
<!-- #################################################### -->

https://exercism.org/mentoring/discussions/4b0554b1a0204a49b0ade0278b8a8e5b

> Upvar makes the same work that global but with more control over the level of stack. Is this correct?
> 
> Is this equivalent?: global c with upvar 0 c

This is a pretty big topic. Tcl uses a stack to maintain "execution frames". The frame will store things like local variables and some sort of execution pointer. When the code calls a procedure, the current frame is paused and a new frame is created and added to the frame stack. Ref [https://wiki.tcl-lang.org/page/stack+frame](https://wiki.tcl-lang.org/page/stack+frame)

From the perspective of the `uplevel` and `upvar` commands, the **current frame** is `0`, the **caller's frame** is `1`, and so on up to the initial ("global") frame. _At the same time_, the **global frame** is `#0`, the next frame in the stack is `#1` and so on down to the current frame.

In a proc, these two commands are identical:
```tcl
global foo
# and
upvar #0 foo foo
```

Now `variable` is different because we're dealing with namespaces.

* in the context of a `namespace eval name {...}` the `variable` command is like `set`, initializing the variable inside the namespace.
* in the context of a procedure in that same namespace, `variable` is like `global`, linking the variable to the namespace level.

---
<!-- #################################################### -->

## TclOO Notes

in very broad brush strokes, classes and instances are implemented as Tcl
namespace ensembles.
* instance variables are thus namespace variables
* instance methods are namespace procedures
* instance methods are exported (or not) to achieve public/private
    visibility.

by default, all methods that start with a lower case letter are exported
  (i.e. "public").  
* other methods need to be explicitly exported. Example:
    ```tcl
    method == {other} {...}
    export ==
    ```

<!-- -->

Instance variable declarations:
* If you use `variable myVar` **outside** of the constructor, then
  you do **not** need to use `my variable myVar` in any method: 
  the class knows how to find `$myVar` in the object's namespace.
* If you use `variable myVar` **inside** the constructor, then
  you **must** use `my variable myVar` in all methods that use
  that variable.

Full details in 
[when to use `my variable x` in a method?](https://stackoverflow.com/q/58071069/7552)
with the author of TclOO providing the answer.

---

Confusingly, Tcl has decided to give the OO `variable` command different arguments then the `namespace variable` command:

* in a namespace, [`variable`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/variable.htm) takes a varname and optionally a value
* in a class, [`variable`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/define.htm) takes only variable names. You give it a value in the constructor or a method.

---
<!-- #################################################### -->

## Optimizations

Avoid copying data in memory. Change
```tcl
set mylist [linsert $mylist 0 some new content]
```
to
```tcl
set mylist [linsert $mylist[set mylist ""] 0 some new content]
```
This dereferences the value of the variable and then sets the variable to
the empty string. This reduces the variable's reference count.

Details at [https://wiki.tcl-lang.org/K](https://wiki.tcl-lang.org/K),
specifically the [Unsharing
Objects](https://wiki.tcl-lang.org/page/K#c2a6014c2d129837889d8a8000d05e5c3b44e8f6b46cab777c04df8a927bfad2)
section, and [this Stack Overflow answer](https://stackoverflow.com/a/64117854/7552)


---
<!-- #################################################### -->

## Links and references

* [Tcl Style Guide](https://core.tcl-lang.org/tips/doc/trunk/tip/352.md)
* [Tcl info page on StackOverflow](https://stackoverflow.com/tags/tcl/info)

### Stack Overflow

* [Q: TclOO: Difference between declaring variable as “class” level or in
  constructor](https://stackoverflow.com/q/58071069/7552)
* [Q: Using `lmap` to filter list of strings](https://stackoverflow.com/q/30489106/7552)
* [classify characters by character class](https://stackoverflow.com/a/73534451/7552)
    - follows [the same effort for bash](https://github.com/glennj/exercism.io/blob/main/_mentor_notes/bash.md#exploring-character-classes)

### Tcl wiki

[Brace your expr-essions](https://wiki.tcl-lang.org/page/Brace+your+expr-essions)

It's best practice to "brace your expr-essions".
This applies to the `expr` command but also `if` and `while`.
See [Brace your expr-essions](https://wiki.tcl-lang.org/page/Brace+your+expr-essions)
and a similar discussion on the [`if` wiki page](https://wiki.tcl-lang.org/page/if#c0af59c387e4e754fbfe4d65236c77b7a9f9e5496baea85a10dfa1447e83f6b7)

Here's a (somewhat contrived) example why it's important:
```tcl
set x 10
if {$x > 0} {
      puts "true $x"
} elseif [set x -5] {
      puts "false $x"
}
# => false -5
```
by the time `{$x > 0}` is being evaluated, the variable may have an unexpected value.

---
## Namespace variables

Referring to [this resistor-color solution](https://exercism.io/mentor/solutions/1c3938443f094fafbcc9eab83b2a526c?iteration_idx=1)

It's important to use the `variable` command to set namespace variables. This code can be defeated by creating a _global_ `colors` variable before sourcing the file:
```tcl
# in a tclsh session
% dict set colors rainbow 12345
rainbow 12345
% source resistor-color.tcl
% resistorColor::colorCode black
can't read "colors": no such variable
```
Add `variable colors` before line 2, and this happens:
```tcl
# in a tclsh session
% dict set colors rainbow 12345
rainbow 12345
% source resistor-color.tcl
% resistorColor::colorCode black
0
% resistorColor::colorCode rainbow
Invalid color: rainbow
% dict get $colors rainbow
12345
```
This is due to [name resolution](http://www.tcl-lang.org/man/tcl8.6/TclCmd/namespace.htm#M26) as discussed on the `namespace` man page.

---
# Exercises

<!-- #################################################### -->

## leap

This is a common anti-pattern:
```tcl
if {cond} {
  return true
} else {
  return false
}
```
`cond` is already an expression that evaluates to a boolean value. You can return it directly.
```tcl
  return [expr {cond}]
```
Or, since Tcl procs will return the value of the last command, you can just end the proc with
```tcl
  expr {cond}
```

---

The challenge here is to combine all the criteria into a single expression using the `&&` `||` and `!` logical operators.

It often helps to say the leap year rules out loud using the words "and" and "or" and "not".

<!-- #################################################### -->

## word-count

In general, it's better to describe the "good" characters of words we want
to keep (letters, numbers and apostrophe), and it's harder to list all the
"bad" characters. What about `|` or `[` or `]`? What about characters that
don't appear on the (US) keyboard like `€` or `é`?

A rule of thumb for Tcl is to use `split` when we know what to throw away
and use `regexp` when we know what we want to keep.

<!-- #################################################### -->

## error-handling

One thing to note: when you're writing a control function that takes a script, it's important to evaluate the script in the caller's scope. This is where the [`uplevel`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/uplevel.htm) command comes in. With `eval` like this code:
```bash
$ tclsh
% source error-handling.tcl
% set a 5
5
% set b 1
1
% handle_error {expr {$a / $b}}
can't read "a": no such variable
```
Change `eval` to `uplevel` and the expected "success" response is obtained.

<!-- #################################################### -->

## clock

To promote code reuse, the `subtract` method could  call `add` with negative minutes.

<!-- -->

<details><summary>For the <code>==</code> method, the TclOO <code>forward</code> command can be used like an "alias" of the <code>equals</code> method. Click for details</summary>

The [`forward` command documentation](http://www.tcl-lang.org/man/tcl8.6/TclCmd/define.htm#M11).

```tcl
forward == my equals
```
Note that, by default, only methods beginning with a lower-case letter are
"exported", or made public. To explicitly export it:
```tcl
export ==
```
(conversely, to make a method private: `unexport aMethod`)
</details>

<!-- #################################################### -->

## two-bucket

Algorithm:

```none
fill start
if other size == goal, fill other
while true {
    if goal is satisfied, return results

    if start is empty, fill start
    else if other is full, empty other
    else pour from start to other
}
```

<!-- #################################################### -->

## two-fer

The ideal would be to have the output string hardcoded only once.
Read the [`proc`](http://www.tcl-lang.org/man/tcl8.6/TclCmd/proc.htm) man
page to learn about how to specify the default value in the argument list.


[man-Tcl]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/Tcl.htm 
[man-contents]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/contents.htm 
[man-if]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/if.htm
[man-while]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/while.htm
[man-for]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/for.htm
[man-expr]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/expr.htm 
[man-try]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/try.htm 
[man-uplevel]: http://www.tcl-lang.org/man/tcl8.6/TclCmd/uplevel.htm 
