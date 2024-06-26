#!/usr/bin/env bats
# generated on 2022-11-02T20:59:04Z
load bats-extra
load bats-jq

@test 'Missed target' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": -9,
          "y": 9
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'On the outer circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": 0,
          "y": 10
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'On the middle circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": -5,
          "y": 0
        }
END_INPUT

    assert_success
    expected=5
    assert_equal "$output" "$expected"
}

@test 'On the inner circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": 0,
          "y": -1
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}

@test 'Exactly on centre' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": 0,
          "y": 0
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}

@test 'Near the centre' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": -0.1,
          "y": -0.1
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}

@test 'Just within the inner circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": 0.7,
          "y": 0.7
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}

@test 'Just outside the inner circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": 0.8,
          "y": -0.8
        }
END_INPUT

    assert_success
    expected=5
    assert_equal "$output" "$expected"
}

@test 'Just within the middle circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": -3.5,
          "y": 3.5
        }
END_INPUT

    assert_success
    expected=5
    assert_equal "$output" "$expected"
}

@test 'Just outside the middle circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": -3.6,
          "y": -3.6
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'Just within the outer circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": -7,
          "y": 7
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'Just outside the outer circle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": 7.1,
          "y": -7.1
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Asymmetric position between the inner and middle circles' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f darts.jq << 'END_INPUT'
        {
          "x": 0.5,
          "y": -4
        }
END_INPUT

    assert_success
    expected=5
    assert_equal "$output" "$expected"
}

