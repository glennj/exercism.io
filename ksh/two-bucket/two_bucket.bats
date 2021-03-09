#!/usr/bin/env bats
#
# vim: ft=sh

: "${BATS_ADDON_DIR:=/usr/local/lib}"
load "${BATS_ADDON_DIR}/bats-support/load.bash"
load "${BATS_ADDON_DIR}/bats-assert/load.bash"

@test "Measure using bucket one of size 3 and bucket two of size 5 - start with bucket one" {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 3 5 1 "one"
    assert_success
    assert_output "moves: 4, goalBucket: one, otherBucket: 5"
}

@test "Measure using bucket one of size 3 and bucket two of size 5 - start with bucket two" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 3 5 1 "two"
    assert_success
    assert_output "moves: 8, goalBucket: two, otherBucket: 3"
}

@test "Measure using bucket one of size 7 and bucket two of size 11 - start with bucket one" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 7 11 2 "one"
    assert_success
    assert_output "moves: 14, goalBucket: one, otherBucket: 11"
}

@test "Measure using bucket one of size 7 and bucket two of size 11 - start with bucket two" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 7 11 2 "two"
    assert_success
    assert_output "moves: 18, goalBucket: two, otherBucket: 7"
}

@test "Measure one step using bucket one of size 1 and bucket two of size 3 - start with bucket two" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 1 3 3 "two"
    assert_success
    assert_output "moves: 1, goalBucket: two, otherBucket: 0"
}

@test "Measure using bucket one of size 2 and bucket two of size 3 - start with bucket one and end with bucket two" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 2 3 3 "one"
    assert_success
    assert_output "moves: 2, goalBucket: two, otherBucket: 2"
}

# error cases
@test "goal is too big for the buckets" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 1 2 3 "one"
    assert_failure

    # Partial matching: output _contains_ the string:
    # you can make your error message more specific.
    assert_output --partial "invalid goal"
}

@test "cannot satisfy the goal" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh two_bucket.ksh 6 8 3 "one"
    assert_failure
    assert_output --partial "invalid goal"
}

