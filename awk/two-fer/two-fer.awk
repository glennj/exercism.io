#!/usr/bin/env gawk -f

NF {name = $0}
END {printf "One for %s, one for me.", (typeof(name) == "untyped") ? "you" : name}
