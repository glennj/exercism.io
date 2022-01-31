proc flatten {input} {
    set flattened [concat {*}$input]
    # compare the string representation of the 2 lists.
    if {$flattened eq $input} {
        return $input
    }
    tailcall flatten $flattened
} 

# This exercise highlights the type-less aspect of Tcl:
# there is no distinction between strings and lists.
# The result of `flatten {one "two three" four}`
# will be {one two three four}
