## Welcome back to Tcl

To catch up with Tcl 8.6, you might want to:
* refresh your memory about 
  [the dodekalogue](http://www.tcl-lang.org/man/tcl8.6/TclCmd/Tcl.htm) and the
  [available commands](http://www.tcl-lang.org/man/tcl8.6/TclCmd/contents.htm)
* read about [OO programming in Tcl](https://www.magicsplat.com/articles/oo.html)

## try ... trap

Tcl 8.6 introduced the [`try` command](http://www.tcl-lang.org/man/tcl8.6/TclCmd/try.htm).
This encapsulates a lot of the uses of `catch`

```tcl
try {
    expr {$a / $b}
} trap {ARITH DIVZERO} {
    puts "division by zero"
}
```
Now, where is "ARITH DIVZERO" documented? I think only in the source code,
but we can as tcl to show us:
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

<!-- -->
