#!/usr/bin/env tclsh
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

############################################################
source "dot-dsl.tcl"


proc createTempFile {contents} {
    set fh [file tempfile filename "./graph.gv"]
    puts $fh $contents
    close $fh
    return $filename
}


test dot-1.1 "empty graph"  -setup {
    set filename [createTempFile {
        graph {
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {nodes {} edges {} attrs {}}

skip dot-1.2
test dot-1.2 "graph with one node"  -setup {
    set filename [createTempFile {
        graph {
            a;
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {nodes {a {}} edges {} attrs {}}

skip dot-1.3
test dot-1.3 "graph with one node with attribute"  -setup {
    set filename [createTempFile {
        graph {
            a [color=green];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {nodes {a {color green}} edges {} attrs {}}

skip dot-1.4
test dot-1.4 "graph with one edge"  -setup {
    set filename [createTempFile {
        graph {
            a -- b;
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {nodes {a {} b {}} edges {{a b} {}} attrs {}}

skip dot-1.5
test dot-1.5 "graph with one attribute"  -setup {
    set filename [createTempFile {
        graph {
            [foo=1];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {nodes {} edges {} attrs {foo 1}}

skip dot-1.6
test dot-1.6 "graph with comments"  -setup {
    set filename [createTempFile {
        graph {
            // a C-like comment
            # a shell-like comment
            [foo=1];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {nodes {} edges {} attrs {foo 1}}

skip dot-1.7
test dot-1.7 "graph with lots of stuff"  -setup {
    set filename [createTempFile {
        graph {
            [foo=1];
            [title="Testing Attrs"];
            a [color=green];
            b [label="Beta!"];
            b -- c;
            a -- b [color=blue];
            [bar=true];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {
    nodes {a {color green} b {label "Beta!"} c {}}
    edges {{a b} {color blue} {b c} {}}
    attrs {foo 1 title "Testing Attrs" bar true}
}

skip dot-1.8
test dot-1.8 "multiple edges on one line"  -setup {
    set filename [createTempFile {
        graph {
            a -- b -- c -- d [style=dotted];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {
    nodes {a {} b {} c {} d {}}
    edges {
        {a b} {style dotted}
        {b c} {style dotted}
        {c d} {style dotted}
    }
    attrs {}
}

skip dot-1.9
test dot-1.9 "only 1 edge between nodes"  -setup {
    set filename [createTempFile {
        graph {
            a -- b
            a -- b
            b -- a [color=blue]
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes ok -match dictionary -result {
    nodes {a {} b {}}
    edges {{a b} {color blue}}
    attrs {}
}

skip dot-2.1
test dot-2.1 "malformed input" -setup {
    set filename [createTempFile {
        graphical {
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid command name "graphical"}

skip dot-2.2
test dot-2.2 "malicious input" -setup {
    set filename [createTempFile {
        exec rm some_file
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid command name "exec"}

skip dot-2.3a
test dot-2.3a "malformed edge" -setup {
    set filename [createTempFile {
        graph {
            a --;
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid edge}

skip dot-2.3b
test dot-2.3b "malformed edge 2" -setup {
    set filename [createTempFile {
        graph {
            a b;
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid edge}

skip dot-2.4
test dot-2.4 "invalid edge type" -setup {
    set filename [createTempFile {
        graph {
            a == b;
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid edge}

skip dot-2.5a
test dot-2.5a "malformed multiple edges" -setup {
    set filename [createTempFile {
        graph {
            a -- b --;
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid edge}

skip dot-2.5b
test dot-2.5b "malformed multiple edges" -setup {
    set filename [createTempFile {
        graph {
            a -- b c;
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid edge}

skip dot-2.6
test dot-2.6 "empty attribute" -setup {
    set filename [createTempFile {
        graph {
            a [];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid attribute}

skip dot-2.7
test dot-2.7 "malformed attribute" -setup {
    set filename [createTempFile {
        graph {
            a [name];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid attribute}

skip dot-2.8
test dot-2.8 "empty attribute name" -setup {
    set filename [createTempFile {
        graph {
            a [=value];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {invalid attribute}

skip dot-2.9
test dot-2.9 "non-alphanumeric node name" -setup {
    set filename [createTempFile {
        graph {
            ? [key=value];
        }
    }]
} -body {
    processDotFile $filename
} -cleanup {
    file delete $filename
} -returnCodes error -result {node name must be alphanumeric}

cleanupTests
