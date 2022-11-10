#!/usr/bin/env bats
# generated on 2022-11-02T20:59:53Z
load bats-extra

@test 'Measure using bucket one of size 3 and bucket two of size 5 - start with bucket one' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 3,
          "bucketTwo": 5,
          "goal": 1,
          "startBucket": "one"
        }
END_INPUT

    assert_success
    expected='{"moves":4,"goalBucket":"one","otherBucket":5}'
    assert_equal "$output" "$expected"
}

@test 'Measure using bucket one of size 3 and bucket two of size 5 - start with bucket two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 3,
          "bucketTwo": 5,
          "goal": 1,
          "startBucket": "two"
        }
END_INPUT

    assert_success
    expected='{"moves":8,"goalBucket":"two","otherBucket":3}'
    assert_equal "$output" "$expected"
}

@test 'Measure using bucket one of size 7 and bucket two of size 11 - start with bucket one' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 7,
          "bucketTwo": 11,
          "goal": 2,
          "startBucket": "one"
        }
END_INPUT

    assert_success
    expected='{"moves":14,"goalBucket":"one","otherBucket":11}'
    assert_equal "$output" "$expected"
}

@test 'Measure using bucket one of size 7 and bucket two of size 11 - start with bucket two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 7,
          "bucketTwo": 11,
          "goal": 2,
          "startBucket": "two"
        }
END_INPUT

    assert_success
    expected='{"moves":18,"goalBucket":"two","otherBucket":7}'
    assert_equal "$output" "$expected"
}

@test 'Measure one step using bucket one of size 1 and bucket two of size 3 - start with bucket two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 1,
          "bucketTwo": 3,
          "goal": 3,
          "startBucket": "two"
        }
END_INPUT

    assert_success
    expected='{"moves":1,"goalBucket":"two","otherBucket":0}'
    assert_equal "$output" "$expected"
}

@test 'Measure using bucket one of size 2 and bucket two of size 3 - start with bucket one and end with bucket two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 2,
          "bucketTwo": 3,
          "goal": 3,
          "startBucket": "one"
        }
END_INPUT

    assert_success
    expected='{"moves":2,"goalBucket":"two","otherBucket":2}'
    assert_equal "$output" "$expected"
}

@test 'Not possible to reach the goal' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 6,
          "bucketTwo": 15,
          "goal": 5,
          "startBucket": "one"
        }
END_INPUT

    assert_failure
    expected='impossible'
    assert_equal "$output" "$expected"
}

@test 'With the same buckets but a different goal, then it is possible' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 6,
          "bucketTwo": 15,
          "goal": 9,
          "startBucket": "one"
        }
END_INPUT

    assert_success
    expected='{"moves":10,"goalBucket":"two","otherBucket":0}'
    assert_equal "$output" "$expected"
}

@test 'Goal larger than both buckets is impossible' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f two-bucket.jq << 'END_INPUT'
        {
          "bucketOne": 5,
          "bucketTwo": 7,
          "goal": 8,
          "startBucket": "one"
        }
END_INPUT

    assert_failure
    expected='impossible'
    assert_equal "$output" "$expected"
}

