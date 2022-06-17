#!/usr/bin/env gawk -f

@include "arrays"

BEGIN {
    # We only care about bracket characters, nothing else.
    # So we can define what's allowed in a field
    FPAT = "[][(){}]"

    arrays::init(stack)
}

function yep() {
    print "true"
    next
}

function nope() {
    print "false"
    next
}

{
    arrays::clear(stack)

    for (i = 1; i <= NF; i++) {
        if ($i ~ /[[({]/)
            arrays::push(stack, $i)
        else {
            if (arrays::isempty(stack)) nope()
            switch (arrays::pop(stack)) {
                case "[": if ($i != "]") nope(); break
                case "(": if ($i != ")") nope(); break
                case "{": if ($i != "}") nope(); break
            }
        }
    }

    if (arrays::isempty(stack)) yep(); else nope()
}
