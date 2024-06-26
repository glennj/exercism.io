#!/usr/bin/env bats
load bats-extra
load bats-jq

assert_key_value() {
    local key=$1 expected=$2 actual
    actual=$(jq -rc --arg key "$key" '.[$key]' <<< "$output")
    assert_equal "$actual" "$expected"
}

@test last_week {
    ## task 1
    run jq -f bird-count.jq bird-counting-data.json
    assert_success
    assert_key_value 'last_week' '[0,2,5,3,7,8,4]'
}

@test yesterday_for_disappointing_week {
    ## task 2
    run jq -f bird-count.jq <<< '[[8, 8, 9, 5, 4, 7, 10],[0, 0, 1, 0, 0, 1, 0]]'
    assert_success
    assert_key_value 'yesterday' 1
}

@test yesterday_for_busy_week {
    ## task 2
    run jq -f bird-count.jq <<< '[[0, 0, 1, 0, 0, 1, 0],[8, 8, 9, 5, 4, 7, 10]]'
    assert_success
    assert_key_value 'yesterday' 7
}

@test total_for_disappointing_week {
    ## task 3
    run jq -f bird-count.jq <<< '[[8, 8, 9, 5, 4, 7, 10],[0, 0, 1, 0, 0, 1, 0]]'
    assert_success
    assert_key_value 'total' 2
}

@test total_for_busy_week {
    ## task 3
    run jq -f bird-count.jq <<< '[[0, 0, 1, 0, 0, 1, 0],[5, 9, 12, 6, 8, 8, 17]]'
    assert_success
    assert_key_value 'total' 65
}

@test busy_days_for_disappointing_week {
    ## task 4
    run jq -f bird-count.jq <<< '[[8, 8, 9, 5, 4, 7, 10],[1, 1, 1, 0, 0, 0, 0]]'
    assert_success
    assert_key_value 'busy_days' 0
}

@test busy_days_for_busy_week {
    ## task 4
    run jq -f bird-count.jq <<< '[[1, 1, 1, 0, 0, 0, 0],[4, 9, 5, 7, 8, 8, 2]]'
    assert_success
    assert_key_value 'busy_days' 5
}

@test has_day_without_birds {
    ## task 5
    run jq -f bird-count.jq <<< '[[4, 9, 5, 7, 8, 8, 2],[5, 5, 4, 0, 7, 6]]'
    assert_success
    assert_key_value 'has_day_without_birds' true
}

@test has_day_without_birds_with_no_day_without_birds {
    ## task 5
    run jq -f bird-count.jq <<< '[[1, 1, 1, 0, 0, 0, 0],[4, 5, 9, 10, 9, 4, 3]]'
    assert_success
    assert_key_value 'has_day_without_birds' false
}
