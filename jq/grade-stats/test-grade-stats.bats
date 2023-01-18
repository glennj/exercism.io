#!/usr/bin/env bats
load bats-extra

assert_key_value() {
    local key=$1 expected=$2 actual
    actual=$(jq -rc --arg key "$key" '.[$key]' <<< "$output")
    assert_equal "$actual" "$expected"
}

@test 'number to letter grade A' {
    ## task 1
    run jq -nc '
        include "grade-stats";
        [ range(90; 101) | letter_grade ]
    '
    assert_success
    assert_output '["A","A","A","A","A","A","A","A","A","A","A"]'
}

@test 'number to letter grade B' {
    ## task 1
    run jq -nc '
        include "grade-stats";
        [ range(80; 90) | letter_grade ]
    '
    assert_success
    assert_output '["B","B","B","B","B","B","B","B","B","B"]'
}

@test 'number to letter grade C' {
    ## task 1
    run jq -nc '
        include "grade-stats";
        [ range(70; 80) | letter_grade ]
    '
    assert_success
    assert_output '["C","C","C","C","C","C","C","C","C","C"]'
}

@test 'number to letter grade D' {
    ## task 1
    run jq -nc '
        include "grade-stats";
        [ range(60; 70) | letter_grade ]
    '
    assert_success
    assert_output '["D","D","D","D","D","D","D","D","D","D"]'
}

@test 'number to letter grade F' {
    ## task 1
    run jq -nc '
        include "grade-stats";
        [ range(0; 60) | letter_grade ]
        | (length == 60 and all(. == "F"))
    '
    assert_success
    assert_output 'true'
}

@test 'aggregate the number of students by letter grade' {
    ## task 2
    run jq -c '
        include "grade-stats";
        count_letter_grades
    ' grades.json
    assert_success
    assert_key_value "A" 6 "$output"
    assert_key_value "B" 1 "$output"
    assert_key_value "C" 6 "$output"
    assert_key_value "D" 2 "$output"
    assert_key_value "F" 5 "$output"
}

@test 'aggregate, all letter grades are present even if zero students' {
    ## task 2
    run jq -c '
        include "grade-stats";
        count_letter_grades
    ' <<< '{"alpha": 95, "bravo": 88}'
    assert_success
    assert_key_value "A" 1 "$output"
    assert_key_value "B" 1 "$output"
    assert_key_value "C" 0 "$output"
    assert_key_value "D" 0 "$output"
    assert_key_value "F" 0 "$output"
}
