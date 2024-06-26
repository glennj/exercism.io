#!/usr/bin/env bats
# generated on 2022-11-02T20:59:54Z
load bats-extra
load bats-jq

@test 'no name given' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f two-fer.jq << 'END_INPUT'
        {
          "name": null
        }
END_INPUT

    assert_success
    expected='One for you, one for me.'
    assert_equal "$output" "$expected"
}

@test 'a name given' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f two-fer.jq << 'END_INPUT'
        {
          "name": "Alice"
        }
END_INPUT

    assert_success
    expected='One for Alice, one for me.'
    assert_equal "$output" "$expected"
}

@test 'another name given' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f two-fer.jq << 'END_INPUT'
        {
          "name": "Bob"
        }
END_INPUT

    assert_success
    expected='One for Bob, one for me.'
    assert_equal "$output" "$expected"
}

