# Trying to reuse these custom procs as much as possible.
#
# Core list functionality used:
# - `foreach` to iterate over a list
# - `{}` to construct an empty list
# - `list` to construct a list with elements
# - `{*}` to spread a list
#
namespace eval listOps {}

proc listOps::append {listname values} {
    upvar 1 $listname list
    set list [list {*}$list {*}$values]
}

proc listOps::foldl {list initial func} {
    set accum $initial
    foreach elem $list {
        set accum [uplevel 1 [list apply $func $accum $elem]]
    }
    return $accum
}

proc listOps::foldr {list initial func} {
    set accum $initial
    foreach elem [listOps::reverse $list] {
        set accum [uplevel 1 [list apply $func $elem $accum]]
        # order of args is swapped:            ^^^^^ ^^^^^^
    }
    return $accum
}

# just about everything else can be implemented with foldl

proc listOps::reverse {list} {
    listOps::foldl $list {} {{reversed elem} {
        list $elem {*}$reversed
    }}
}

proc listOps::concat {listOfLists} {
    listOps::foldl $listOfLists {} {{accum list} {
        listOps::append accum $list
        set accum
    }}
}

proc listOps::length {list} {
    listOps::foldl $list 0 {{len _} {expr {$len + 1}}}
}

proc listOps::filter {list func} {
    # ugh,  a bit of quoting hell here: we want to substitute 
    # this proc's $func but we have to protect the elem variable
    # and all the command substitutions.
    #
    # using a string template and the `format` command.
    #
    set foldFunc {{filtered elem} {
        if {[uplevel 1 [list apply {%s} $elem]]} {
            listOps::append filtered $elem
        }
        set filtered
    }}

    listOps::foldl $list {} [format $foldFunc $func]

    # the more straightforward way to do it
    #
    ## set filtered {}
    ## foreach elem $list {
    ##     if {[uplevel 1 [list apply $func $elem]]} {
    ##         listOps::append filtered $elem
    ##     }
    ## }
    ## return $filtered
}

proc listOps::map {list func} {
    # same commentary as above
    set foldFunc {{mapped elem} {
        listOps::append mapped [uplevel 1 [list apply {%s} $elem]]
        set mapped
    }}

    listOps::foldl $list {} [format $foldFunc $func]

    # the more straightforward way to do it
    #
    ## set mapped {}
    ## foreach elem $list {
    ##     listOps::append mapped [uplevel 1 [list apply $func $elem]]
    ## }
    ## return $mapped
}
