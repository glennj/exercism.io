#!/usr/bin/env tclsh

rename unknown __tcl_unknown

proc unknown {args} {
    tailcall {*}[switch -exact -- [lindex $args 0] {
        "Hello," { list join $args }
        "hello"  { list Hello, World! }
        default  { list __tcl_unknown {*}$args }
    }]
}

