#!/usr/bin/env bats
# generated on 
load bats-extra

@test 'finds a value in an array with one element' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            6
          ],
          "value": 6
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'finds a value in the middle of an array' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            4,
            6,
            8,
            9,
            11
          ],
          "value": 6
        }
END_INPUT

    assert_success
    expected=3
    assert_equal "$output" "$expected"
}

@test 'finds a value at the beginning of an array' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            4,
            6,
            8,
            9,
            11
          ],
          "value": 1
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'finds a value at the end of an array' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            4,
            6,
            8,
            9,
            11
          ],
          "value": 11
        }
END_INPUT

    assert_success
    expected=6
    assert_equal "$output" "$expected"
}

@test 'finds a value in an array of odd length' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            5,
            8,
            13,
            21,
            34,
            55,
            89,
            144,
            233,
            377,
            634
          ],
          "value": 144
        }
END_INPUT

    assert_success
    expected=9
    assert_equal "$output" "$expected"
}

@test 'finds a value in an array of even length' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            5,
            8,
            13,
            21,
            34,
            55,
            89,
            144,
            233,
            377
          ],
          "value": 21
        }
END_INPUT

    assert_success
    expected=5
    assert_equal "$output" "$expected"
}

@test 'identifies that a value is not included in the array' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            4,
            6,
            8,
            9,
            11
          ],
          "value": 7
        }
END_INPUT

    assert_failure
    expected='value not in array'
    assert_equal "$output" "$expected"
}

@test 'a value smaller than the array'\''s smallest value is not found' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            4,
            6,
            8,
            9,
            11
          ],
          "value": 0
        }
END_INPUT

    assert_failure
    expected='value not in array'
    assert_equal "$output" "$expected"
}

@test 'a value larger than the array'\''s largest value is not found' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            3,
            4,
            6,
            8,
            9,
            11
          ],
          "value": 13
        }
END_INPUT

    assert_failure
    expected='value not in array'
    assert_equal "$output" "$expected"
}

@test 'nothing is found in an empty array' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f binary-search.jq << 'END_INPUT'
        {
          "array": [],
          "value": 1
        }
END_INPUT

    assert_failure
    expected='value not in array'
    assert_equal "$output" "$expected"
}

@test 'nothing is found when the left and right bounds cross' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f binary-search.jq << 'END_INPUT'
        {
          "array": [
            1,
            2
          ],
          "value": 0
        }
END_INPUT

    assert_failure
    expected='value not in array'
    assert_equal "$output" "$expected"
}
