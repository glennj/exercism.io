#!/usr/bin/env bats
load bats-extra
load bats-jq

assert_float() {
    local OPTIND OPTARG
    local decimals=2 actual expected
    while getopts :d: opt; do
        case $opt in
            d) decimals=$OPTARG ;;
            *) return 2 ;;
        esac
    done
    shift $((OPTIND - 1))
    # bash can't do floating point: use awk (also uses IEEE754 numbers)
    read -r actual expected < <(
        awk -v d="$decimals" -v a="$1" -v e="$2" '
            BEGIN {
                m = 10 ^ d
                print int(a * m)/m, int(e * m)/m
            }
        '
    )
    # now call a bats-assert command to get the desired output
    assert_equal "$actual" "$expected"
}

@test production_rate_per_hour_for_speed_zero {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 0}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 0
}

@test production_rate_per_hour_for_speed_one {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 1}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 221.0
}

@test production_rate_per_hour_for_speed_two {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 2}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 442.0
}

@test production_rate_per_hour_for_speed_three {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 3}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 663.0
}

@test production_rate_per_hour_for_speed_four {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 4}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 884.0
}

@test production_rate_per_hour_for_speed_five {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 5}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 994.5
}

@test production_rate_per_hour_for_speed_six {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 6}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 1193.4
}

@test production_rate_per_hour_for_speed_seven {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 7}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 1392.3
}

@test production_rate_per_hour_for_speed_eight {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 8}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 1591.2
}

@test production_rate_per_hour_for_speed_nine {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 9}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 1591.2
}

@test production_rate_per_hour_for_speed_ten {
    ## task 1
    run jq -f assembly-line.jq <<< '{"speed": 10}'
    assert_success
    assert_float -d 4 -- "${lines[0]}" 1701.7
}

@test working_items_per_minute_for_speed_one {
    ## task 2
    run jq -f assembly-line.jq <<< '{"speed": 1}'
    assert_success
    assert_line --index 1 3
}

@test working_items_per_minute_for_speed_five {
    ## task 2
    run jq -f assembly-line.jq <<< '{"speed": 5}'
    assert_success
    assert_line --index 1 16
}

@test working_items_per_minute_for_speed_eight {
    ## task 2
    run jq -f assembly-line.jq <<< '{"speed": 8}'
    assert_success
    assert_line --index 1 26
}

@test working_items_per_minute_for_speed_nine {
    ## task 2
    run jq -f assembly-line.jq <<< '{"speed": 9}'
    assert_success
    assert_line --index 1 26
}

@test working_items_per_minute_for_speed_ten {
    ## task 2
    run jq -f assembly-line.jq <<< '{"speed": 10}'
    assert_success
    assert_line --index 1 28
}
