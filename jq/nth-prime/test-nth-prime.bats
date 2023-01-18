#!/usr/bin/env bats
# generated on 2022-11-02T20:59:29Z
load bats-extra

@test 'first prime' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -n -r -f nth-prime.jq --argjson n 1

    assert_success
    expected=2
    assert_equal "$output" "$expected"
}

@test 'second prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -n -r -f nth-prime.jq --argjson n 2

    assert_success
    expected=3
    assert_equal "$output" "$expected"
}

@test 'sixth prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -n -r -f nth-prime.jq --argjson n 6

    assert_success
    expected=13
    assert_equal "$output" "$expected"
}

@test 'big prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -n -r -f nth-prime.jq --argjson n 10001

    assert_success
    expected=104743
    assert_equal "$output" "$expected"
}

@test 'there is no zeroth prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -n -r -f nth-prime.jq --argjson n 0

    assert_failure
    expected='there is no zeroth prime'
    assert_equal "$output" "$expected"
}

