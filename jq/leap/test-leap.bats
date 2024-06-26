#!/usr/bin/env bats
# generated on 2022-11-02T20:59:17Z
load bats-extra
load bats-jq

@test 'year not divisible by 4 in common year' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 2015
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'year divisible by 2, not divisible by 4 in common year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 1970
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'year divisible by 4, not divisible by 100 in leap year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 1996
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'year divisible by 4 and 5 is still a leap year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 1960
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'year divisible by 100, not divisible by 400 in common year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 2100
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'year divisible by 100 but not by 3 is still not a leap year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 1900
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'year divisible by 400 is leap year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 2000
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'year divisible by 400 but not by 125 is still a leap year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 2400
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'year divisible by 200, not divisible by 400 in common year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f leap.jq << 'END_INPUT'
        {
          "year": 1800
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

