#!/usr/bin/env bats
load bats-extra

assert_key_value() {
    local result
    result=$(echo "$output" | jq -r --arg key "$2" --argjson val "$1" '.[$key] == $val')
    [[ $result == "true" ]]
}

@test "Sum of empty array" {
    ## task 1
    run jq -n '
        include "recursive-functions";
        [] | array_add
    '
    assert_success
    assert_output 0
}

@test "Sum of array of one number" {
    ## task 1
    run jq -n '
        include "recursive-functions";
        [42] | array_add
    '
    assert_success
    assert_output 42
}

@test "Sum of array of numbers" {
    ## task 1
    run jq -n '
        include "recursive-functions";
        [1, 2, 3, 4] | array_add
    '
    assert_success
    assert_output 10
}


@test "Reverse an empty array" {
    ## task 2
    run jq -cn '
        include "recursive-functions";
        [] | array_reverse
    '
    assert_success
    assert_output '[]'
}

@test "Reverse an array of one number" {
    ## task 2
    run jq -cn '
        include "recursive-functions";
        [42] | array_reverse
    '
    assert_success
    assert_output '[42]'
}

@test "Reverse an array of numbers" {
    ## task 2
    run jq -cn '
        include "recursive-functions";
        [1, 2, 3, 4] | array_reverse
    '
    assert_success
    assert_output '[4,3,2,1]'
}

@test "Map an empty array" {
    ## task 3
    run jq -cn '
        include "recursive-functions";
        [] | array_map(. * 2)
    '
    assert_success
    assert_output '[]'
}

@test "Map an array of one number" {
    ## task 3
    run jq -cn '
        include "recursive-functions";
        [42] | array_map(. * 2)
    '
    assert_success
    assert_output '[84]'
}

@test "Map an array of numbers" {
    ## task 3
    run jq -cn '
        include "recursive-functions";
        [1, 2, 3, 4] | array_map(. * 2)
    '
    assert_success
    assert_output '[2,4,6,8]'
}



# vim: sw=4 ts=8
