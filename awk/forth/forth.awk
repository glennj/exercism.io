#!/usr/bin/env gawk -f

@include "lib_forth"

BEGINFILE {
    forth = Forth::new()
}
{
    Forth::evaluate(forth, $0)
}
ENDFILE {
    print Forth::toString(forth)
    Forth::destroy(forth)
}
