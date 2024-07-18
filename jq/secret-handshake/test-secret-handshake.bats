#!/usr/bin/env bats
# generated on 2024-07-12T19:12:00Z
load bats-extra
load bats-jq

@test 'wink for 1' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 1
        }
END_INPUT

    assert_success
    expected='["wink"]'
    assert_equal "$output" "$expected"
}

@test 'double blink for 10' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 2
        }
END_INPUT

    assert_success
    expected='["double blink"]'
    assert_equal "$output" "$expected"
}

@test 'close your eyes for 100' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 4
        }
END_INPUT

    assert_success
    expected='["close your eyes"]'
    assert_equal "$output" "$expected"
}

@test 'jump for 1000' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 8
        }
END_INPUT

    assert_success
    expected='["jump"]'
    assert_equal "$output" "$expected"
}

@test 'combine two actions' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 3
        }
END_INPUT

    assert_success
    expected='["wink","double blink"]'
    assert_equal "$output" "$expected"
}

@test 'reverse two actions' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 19
        }
END_INPUT

    assert_success
    expected='["double blink","wink"]'
    assert_equal "$output" "$expected"
}

@test 'reversing one action gives the same action' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 24
        }
END_INPUT

    assert_success
    expected='["jump"]'
    assert_equal "$output" "$expected"
}

@test 'reversing no actions still gives no actions' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 16
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'all possible actions' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 15
        }
END_INPUT

    assert_success
    expected='["wink","double blink","close your eyes","jump"]'
    assert_equal "$output" "$expected"
}

@test 'reverse all possible actions' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 31
        }
END_INPUT

    assert_success
    expected='["jump","close your eyes","double blink","wink"]'
    assert_equal "$output" "$expected"
}

@test 'do nothing for zero' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f secret-handshake.jq << 'END_INPUT'
        {
          "number": 0
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}
