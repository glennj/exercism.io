#!/usr/bin/env bats
# generated on 2024-06-07T22:08:03Z
load bats-extra
load bats-jq

@test 'Yacht' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            5,
            5,
            5,
            5,
            5
          ],
          "category": "yacht"
        }
END_INPUT

    assert_success
    expected=50
    assert_equal "$output" "$expected"
}

@test 'Not Yacht' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            3,
            3,
            2,
            5
          ],
          "category": "yacht"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Ones' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            1,
            1,
            3,
            5
          ],
          "category": "ones"
        }
END_INPUT

    assert_success
    expected=3
    assert_equal "$output" "$expected"
}

@test 'Ones, out of order' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            3,
            1,
            1,
            5,
            1
          ],
          "category": "ones"
        }
END_INPUT

    assert_success
    expected=3
    assert_equal "$output" "$expected"
}

@test 'No ones' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            4,
            3,
            6,
            5,
            5
          ],
          "category": "ones"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Twos' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            2,
            3,
            4,
            5,
            6
          ],
          "category": "twos"
        }
END_INPUT

    assert_success
    expected=2
    assert_equal "$output" "$expected"
}

@test 'Fours' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            4,
            1,
            4,
            1
          ],
          "category": "fours"
        }
END_INPUT

    assert_success
    expected=8
    assert_equal "$output" "$expected"
}

@test 'Yacht counted as threes' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            3,
            3,
            3,
            3,
            3
          ],
          "category": "threes"
        }
END_INPUT

    assert_success
    expected=15
    assert_equal "$output" "$expected"
}

@test 'Yacht of 3s counted as fives' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            3,
            3,
            3,
            3,
            3
          ],
          "category": "fives"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Fives' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            5,
            3,
            5,
            3
          ],
          "category": "fives"
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}

@test 'Sixes' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            2,
            3,
            4,
            5,
            6
          ],
          "category": "sixes"
        }
END_INPUT

    assert_success
    expected=6
    assert_equal "$output" "$expected"
}

@test 'Full house two small, three big' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            2,
            2,
            4,
            4,
            4
          ],
          "category": "full house"
        }
END_INPUT

    assert_success
    expected=16
    assert_equal "$output" "$expected"
}

@test 'Full house three small, two big' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            5,
            3,
            3,
            5,
            3
          ],
          "category": "full house"
        }
END_INPUT

    assert_success
    expected=19
    assert_equal "$output" "$expected"
}

@test 'Two pair is not a full house' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            2,
            2,
            4,
            4,
            5
          ],
          "category": "full house"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Four of a kind is not a full house' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            4,
            4,
            4,
            4
          ],
          "category": "full house"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Yacht is not a full house' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            2,
            2,
            2,
            2,
            2
          ],
          "category": "full house"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Four of a Kind' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            6,
            6,
            4,
            6,
            6
          ],
          "category": "four of a kind"
        }
END_INPUT

    assert_success
    expected=24
    assert_equal "$output" "$expected"
}

@test 'Yacht can be scored as Four of a Kind' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            3,
            3,
            3,
            3,
            3
          ],
          "category": "four of a kind"
        }
END_INPUT

    assert_success
    expected=12
    assert_equal "$output" "$expected"
}

@test 'Full house is not Four of a Kind' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            3,
            3,
            3,
            5,
            5
          ],
          "category": "four of a kind"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Little Straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            3,
            5,
            4,
            1,
            2
          ],
          "category": "little straight"
        }
END_INPUT

    assert_success
    expected=30
    assert_equal "$output" "$expected"
}

@test 'Little Straight as Big Straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            2,
            3,
            4,
            5
          ],
          "category": "big straight"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Four in order but not a little straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            1,
            2,
            3,
            4
          ],
          "category": "little straight"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'No pairs but not a little straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            2,
            3,
            4,
            6
          ],
          "category": "little straight"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Minimum is 1, maximum is 5, but not a little straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            1,
            1,
            3,
            4,
            5
          ],
          "category": "little straight"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Big Straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            4,
            6,
            2,
            5,
            3
          ],
          "category": "big straight"
        }
END_INPUT

    assert_success
    expected=30
    assert_equal "$output" "$expected"
}

@test 'Big Straight as little straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            6,
            5,
            4,
            3,
            2
          ],
          "category": "little straight"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'No pairs but not a big straight' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            6,
            5,
            4,
            3,
            1
          ],
          "category": "big straight"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Choice' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            3,
            3,
            5,
            6,
            6
          ],
          "category": "choice"
        }
END_INPUT

    assert_success
    expected=23
    assert_equal "$output" "$expected"
}

@test 'Yacht as choice' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f yacht.jq << 'END_INPUT'
        {
          "dice": [
            2,
            2,
            2,
            2,
            2
          ],
          "category": "choice"
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}
