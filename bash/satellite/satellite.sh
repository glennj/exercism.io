#!/usr/bin/env bash

source ./utils.bash
checkBashVersion 4.3 "namerefs"

reconstructTree() {
    local -a preorder inorder

    # read space-separated words into arrays
    read -ra preorder <<< "$1"
    read -ra inorder  <<< "$2"

    validateInput preorder inorder

    if ((${#preorder[@]} == 0)); then
        # an empty tree
        echo "{}"
    else
        # first item in the preorder is the tree root
        local root=${preorder[0]}

        # find the index of the root element in the inorder traversal
        local idx
        for ((idx = 0; idx < ${#inorder[@]}; idx++)); do
            if [[ $root == "${inorder[idx]}" ]]; then
                break
            fi
        done

        local preLeft preRight inLeft inRight

        preLeft=("${preorder[@]:1:idx}")
        preRight=("${preorder[@]:idx+1}")

        inLeft=("${inorder[@]:0:idx}")
        inRight=("${inorder[@]:idx+1}")

        # don't hardcode the function name for recursive calls
        local funcname=${FUNCNAME[0]}

        printf '{"v": "%s", "l": %s, "r": %s}' \
            "$root" \
            "$("$funcname" "${preLeft[*]}"  "${inLeft[*]}")" \
            "$("$funcname" "${preRight[*]}" "${inRight[*]}")"
    fi
}

validateInput() {
    local -n pre=$1 in=$2

    assert "${#pre[@]} == ${#in[@]}" "traversals must have the same length"

    local -A elems
    for elem in "${pre[@]}"; do
        elems[$elem]=1
    done
    assert "${#pre[@]} == ${#elems[@]}" "traversals must contain unique elements"

    for elem in "${in[@]}"; do
        assert "elems[$elem] == 1" "traversals must have the same elements"
    done
}

reconstructTree "$@"
