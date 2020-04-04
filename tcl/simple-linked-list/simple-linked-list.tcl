oo::class create Node {
    variable datum
    variable next

    constructor {value} {
        set datum $value
        set next ""
    }

    method datum {} {return $datum}

    method next {{node "-get"}} {
        if {[info object isa typeof $node [self class]]} {
            set next $node
        } else {
            switch -exact -- $node {
                "-get"   {return $next}
                "-unset" {set next ""}
                default  {error "invalid argument for Node next: '$node'"}
            }
        }
        return
    }
}

############################################################
oo::class create SimpleLinkedList {
    variable head

    constructor {{values {}}} {
        set head ""
        foreach value $values {
            my push [Node new $value]
        }
    }

    method push {node} {
        if {$head ne ""} {
            $node next $head
        }
        set head $node
        return [self]
    }

    method pop {} {
        set node $head
        if {$node ne ""} {
            set head [$node next]
            $node next -unset
        }
        return $node
    }

    method peek {} {return $head}

    method foreach {nodeVar body} {
        upvar 1 $nodeVar node
        set node $head
        while {$node ne ""} {
            uplevel 1 $body
            set node [$node next]
        }
        return
    }

    method map {nodeVar body} {
        upvar 1 $nodeVar node
        set result {}
        my foreach node {
            lappend result [uplevel 1 $body]
        }
        return $result
    }

    method toList {} {
        my map node {$node datum}
    }

    method reverse {} {
        set new [[self class] new]
        while {$head ne ""} {
            $new push [my pop]
        }
        set head [$new peek]
        return [self]
    }
}
