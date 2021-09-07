#!/usr/bin/env bash

source ./utils.bash
source ./grains_lib.bash

# pick an implementation:
#namespace="bc"
namespace="bash_only"
#namespace="precalculated"

shopt -s extglob

case $1 in
    total)
        "$namespace"::total "$1"
        ;;
    ?(-)+([0-9]) ) # an integer
        assert "1 <= $1 && $1 <= 64" "Error: invalid input"
        "$namespace"::atSquare "$1"
        ;;
    *)  die "Error: invalid input"
        ;;
esac
