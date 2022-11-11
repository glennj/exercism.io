#!/usr/bin/env bats
# generated on 2022-11-02T20:59:50Z
load bats-extra

@test 'root of 1' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f square-root.jq << 'END_INPUT'
        {
          "radicand": 1
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'root of 4' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f square-root.jq << 'END_INPUT'
        {
          "radicand": 4
        }
END_INPUT

    assert_success
    expected=2
    assert_equal "$output" "$expected"
}

@test 'root of 25' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f square-root.jq << 'END_INPUT'
        {
          "radicand": 25
        }
END_INPUT

    assert_success
    expected=5
    assert_equal "$output" "$expected"
}

@test 'root of 81' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f square-root.jq << 'END_INPUT'
        {
          "radicand": 81
        }
END_INPUT

    assert_success
    expected=9
    assert_equal "$output" "$expected"
}

@test 'root of 196' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f square-root.jq << 'END_INPUT'
        {
          "radicand": 196
        }
END_INPUT

    assert_success
    expected=14
    assert_equal "$output" "$expected"
}

@test 'root of 65025' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f square-root.jq << 'END_INPUT'
        {
          "radicand": 65025
        }
END_INPUT

    assert_success
    expected=255
    assert_equal "$output" "$expected"
}

