#!/usr/bin/env bats
# generated on 2026-06-28T04:19:45+00:00
load bats-extra
load bats-jq

@test '0 eggs' {
    # [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pop-count.jq << 'END_INPUT'
        {
          "number": 0
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test '1 egg' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pop-count.jq << 'END_INPUT'
        {
          "number": 16
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test '4 eggs' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pop-count.jq << 'END_INPUT'
        {
          "number": 89
        }
END_INPUT

    assert_success
    expected=4
    assert_equal "$output" "$expected"
}

@test '13 eggs' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pop-count.jq << 'END_INPUT'
        {
          "number": 2000000000
        }
END_INPUT

    assert_success
    expected=13
    assert_equal "$output" "$expected"
}
