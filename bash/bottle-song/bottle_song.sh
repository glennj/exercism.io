#!/usr/bin/env bash

readonly Numbers=(No One Two Three Four Five Six Seven Eight Nine Ten)

die() { echo "$*" >&2; exit 1; }

bottle() {
    local n=$1 tolower=$2
    local b="${Numbers[n]} green bottle"
    ((n != 1)) && b+="s"
    "$tolower" && b=${b,}
    echo "$b"
}

verse() {
    cat <<END_VERSE
$(bottle "$1" false) hanging on the wall,
$(bottle "$1" false) hanging on the wall,
And if $(bottle 1 true) should accidentally fall,
There'll be $(bottle $(($1 - 1)) true) hanging on the wall.
END_VERSE
}

sing() {
    local n=$1 count=$2
    while true; do
        verse "$n"
        # shellcheck disable=SC2015
        ((--n, --count)) && echo || break
    done
}

#-------------------------------------------------------------
(($# == 2)) || die "2 arguments expected"
(($1 >= $2)) || die "cannot generate more verses than bottles"

sing "$@"
