#!/usr/bin/env gawk -f
function isnull(x) {return typeof(x) == "untyped"}
NF  {name = $0}
END {printf "One for %s, one for me.", isnull(name) ? "you" : name}
