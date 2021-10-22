package provide structures::list 0.1
# TODO documentation!

package require Tcl 8.6-
set {IsTcl8.7+} [package vsatisfies [package provide Tcl] 8.7-]

package require structures::sequenceable

# a (not so) thin wrapper around core Tcl list operations, 
# with a smackerel of new functionality
#
# ref: http://wiki.tcl.tk/K

oo::class create List {
    mixin Sequenceable
    variable data

    constructor {args} {
        set data $args
    }

    method clone {} {[self class] new {*}$data}
    
    # class methods require Tcl 8.7
    if {${IsTcl8.7+}} {
        classmethod repeat {count item} {
            [self] new {*}[lrepeat $count $item]
        }
    }

    # private method used by Sequenceable
    method Data {} {return $data}
    forward list  my Data

    method K {a b} {set a}

    # methods that call out directly to core commands
    method length  {}     {llength  $data}
    method append  {args} {
        lappend  data  {*}$args
        return [self]
    }
    forward add  my append

    method index   {args} {lindex   $data {*}$args}
    method range   {args} {lrange   $data {*}$args}

    method search {args} {
        set pattern [lindex $args end]
        set options [lrange $args 0 end-1]
        lsearch {*}$options $data $pattern
    }
    forward indexOf  my search -exact

    method join {{delim " "}} {join $data $delim}

    # in-place
    method assign {args} {
        foreach name $args {
            upvar 1 $name $name
        }
        set data [lassign [my K $data [set data {}]] {*}$args]
        return [self]
    }
    # in-place
    method insert {args} {
        set data [linsert [my K $data [set data {}]] {*}$args]
        return [self]
    }
    # in-place
    method replace {args} {
        set data [lreplace [my K $data [set data {}]] {*}$args]
        return [self]
    }
    # in-place
    method set {args} {
        lset data {*}$args
        return [self]
    }

    # return the sorted list
    method sorted {args} {lsort {*}$args $data}
    method reversed {} {lreverse $data}

    # in-place
    method sort {args} {
        set data [lsort [my K {*}$args $data [set data {}]]]
        return [self]
    }
    method reverse {} {
        set data [lreverse [my K $data [set data {}]]]
        return [self]
    }

    # additional functionalty
    method isempty {} {expr {[llength $data] == 0}}

    # Tcl 8.7 introduced the `lpop` command
    if {${IsTcl8.7+}} {
        method pop {args} {lpop data {*}$args}
    } else {
        method pop {{idx end}} {
            set item [lindex $data $idx]
            set data [lreplace [my K $data [set data {}]] $idx $idx]
            return $item
        }
    }
    forward push  my append
    forward shift  my pop 0
    forward unshift  my insert 0

    method isaList {obj} {
        info object isa typeof $obj [self class]
    }
    unexport isaList

    method + {aList} {
        set retVal [my clone]
        if {[my isaList $aList]} {
            $retVal append {*}[$aList list]
        } else {
            $retVal append {*}$aList
        }
        return $retVal
    }
    export +
}
